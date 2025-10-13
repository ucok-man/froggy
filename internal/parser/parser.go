package parser

import (
	"fmt"
	"strconv"

	"github.com/ucok-man/froggy/internal/ast"
	"github.com/ucok-man/froggy/internal/lexer"
	"github.com/ucok-man/froggy/internal/token"
)

// ============================================================================
// PRECEDENCE LEVELS
// ============================================================================
// These constants define how tightly operators bind to their operands.
// Higher numbers mean tighter binding (evaluated first).
//
// Example: In "2 + 3 * 4", multiplication (PRODUCT) has higher precedence
// than addition (SUM), so it's evaluated first: 2 + (3 * 4) = 14
const (
	_ int = iota // Skip zero value
	LOWEST
	EQUALS      // ==, !=
	LESSGREATER // <, >
	SUM         // +, -
	PRODUCT     // *, /
	PREFIX      // -x, !x
	CALL        // fn(x)
)

// precedenceTable maps each operator token to its precedence level.
var precedenceTable = map[token.TokenType]int{
	token.EQ:       EQUALS, // Lowest
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
	token.LPAREN:   CALL, // Highest
}

// ============================================================================
// PARSER DEFINITION
// ============================================================================

// PrefixParserFn parses expressions that start with a specific token.
// Example: parsing "-5" where "-" is the prefix operator.
type PrefixParserFn func() ast.Expression

// InfixParserFn parses operators that appear between two expressions.
// Example: parsing "3 + 5" where "+" is the infix operator.
// It receives the left side and returns the complete binary expression.
type InfixParserFn func(leftSide ast.Expression) ast.Expression

// Parser transforms a stream of tokens into an Abstract Syntax Tree (AST).
// It uses Pratt parsing (also called "top-down operator precedence parsing")
// to handle operator precedence elegantly.
type Parser struct {
	lexer  *lexer.Lexer
	errors []string

	currentToken token.Token // The token we're examining right now
	nextToken    token.Token // The token coming up next (for lookahead)

	// These maps tell us how to parse each token type
	prefixParsers map[token.TokenType]PrefixParserFn
	infixParsers  map[token.TokenType]InfixParserFn
}

// New creates a Parser that's ready to parse tokens from the given lexer.
func New(lex *lexer.Lexer) *Parser {
	parser := &Parser{
		lexer:         lex,
		errors:        []string{},
		prefixParsers: make(map[token.TokenType]PrefixParserFn),
		infixParsers:  make(map[token.TokenType]InfixParserFn),
	}

	parser.registerParsingFunctions()

	// Prime the pump: read two tokens so currentToken and nextToken are both set
	parser.advanceToNextToken()
	parser.advanceToNextToken()

	return parser
}

// Errors returns all parsing errors that were encountered.
func (p *Parser) Errors() []string {
	return p.errors
}

// ============================================================================
// PARSER REGISTRATION
// ============================================================================

// registerParsingFunctions sets up the parsing function for each token type.
// This tells the parser "when you see token X, call function Y to parse it".
func (p *Parser) registerParsingFunctions() {
	// PREFIX PARSERS: tokens that can START an expression
	p.registerPrefixParser(token.IDENT, p.parseIdentifier)
	p.registerPrefixParser(token.INT, p.parseIntegerLiteral)
	p.registerPrefixParser(token.TRUE, p.parseBooleanLiteral)
	p.registerPrefixParser(token.FALSE, p.parseBooleanLiteral)
	p.registerPrefixParser(token.BANG, p.parsePrefixOperatorExpression)
	p.registerPrefixParser(token.MINUS, p.parsePrefixOperatorExpression)
	p.registerPrefixParser(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefixParser(token.IF, p.parseIfExpression)
	p.registerPrefixParser(token.FUNCTION, p.parseFunctionLiteral)

	// INFIX PARSERS: operators that appear BETWEEN two expressions
	p.registerInfixParser(token.PLUS, p.parseBinaryOperatorExpression)
	p.registerInfixParser(token.MINUS, p.parseBinaryOperatorExpression)
	p.registerInfixParser(token.SLASH, p.parseBinaryOperatorExpression)
	p.registerInfixParser(token.ASTERISK, p.parseBinaryOperatorExpression)
	p.registerInfixParser(token.EQ, p.parseBinaryOperatorExpression)
	p.registerInfixParser(token.NOT_EQ, p.parseBinaryOperatorExpression)
	p.registerInfixParser(token.LT, p.parseBinaryOperatorExpression)
	p.registerInfixParser(token.GT, p.parseBinaryOperatorExpression)
	p.registerInfixParser(token.LPAREN, p.parseFunctionCallExpression)
}

// registerPrefixParser associates a token type with its prefix parsing function.
func (p *Parser) registerPrefixParser(tokenType token.TokenType, parserFn PrefixParserFn) {
	p.prefixParsers[tokenType] = parserFn
}

// registerInfixParser associates a token type with its infix parsing function.
func (p *Parser) registerInfixParser(tokenType token.TokenType, parserFn InfixParserFn) {
	p.infixParsers[tokenType] = parserFn
}

// ============================================================================
// MAIN PARSING ENTRY POINT
// ============================================================================

// ParseProgram parses the entire source code and returns the root of the AST.
// It keeps parsing statements until it reaches the end of the file.
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{
		Statements: []ast.Statement{},
	}

	for p.currentToken.Type != token.EOF {
		statement := p.parseStatement()
		if statement != nil {
			program.Statements = append(program.Statements, statement)
		}
		p.advanceToNextToken()
	}

	return program
}

// ============================================================================
// STATEMENT PARSING
// ============================================================================

// parseStatement figures out what kind of statement we're looking at
// and delegates to the appropriate specialized parser.
func (p *Parser) parseStatement() ast.Statement {
	switch p.currentToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

// parseLetStatement parses variable declarations.
// Format: let <identifier> = <expression>;
// Example: let x = 5;
func (p *Parser) parseLetStatement() ast.Statement {
	letToken := p.currentToken

	if !p.expectNextTokenToBe(token.IDENT) {
		return nil
	}
	identifierName := p.buildIdentifierFromCurrentToken()

	if !p.expectNextTokenToBe(token.ASSIGN) {
		return nil
	}

	p.advanceToNextToken()
	valueExpression := p.parseExpression(LOWEST)

	p.skipSemicolonIfPresent()

	return &ast.LetStatement{
		Token: letToken,
		Name:  identifierName,
		Value: valueExpression,
	}
}

// parseReturnStatement parses return statements.
// Format: return <expression>;
// Example: return x + 5;
func (p *Parser) parseReturnStatement() ast.Statement {
	returnToken := p.currentToken

	p.advanceToNextToken()
	returnValue := p.parseExpression(LOWEST)

	p.skipSemicolonIfPresent()

	return &ast.ReturnStatement{
		Token:       returnToken,
		ReturnValue: returnValue,
	}
}

// parseExpressionStatement parses expressions that stand alone as statements.
// Format: <expression>;
// Example: x + 5;
func (p *Parser) parseExpressionStatement() ast.Statement {
	statementToken := p.currentToken
	expression := p.parseExpression(LOWEST)

	p.skipSemicolonIfPresent()

	return &ast.ExpressionStatement{
		Token:      statementToken,
		Expression: expression,
	}
}

// ============================================================================
// EXPRESSION PARSING (Pratt Parser Core)
// ============================================================================

// parseExpression is the heart of the Pratt parser algorithm.
// It handles operator precedence by "climbing" up precedence levels.
//
// How it works:
//
// 1. Parse the prefix (start) of the expression
//
// 2. While we see operators with higher precedence, keep parsing
//
// 3. Each operator gets its left side and parses its right side
//
// Example: "1 + 2 * 3":
//
// - Parse "1" (prefix)
//
// - See "+", parse "2 * 3" as right side (because * has higher precedence)
//
// - Result: (1 + (2 * 3))
func (p *Parser) parseExpression(minimumPrecedence int) ast.Expression {
	// Step 1: Parse the beginning of the expression (prefix part)
	prefixParserFn := p.prefixParsers[p.currentToken.Type]
	if prefixParserFn == nil {
		p.reportMissingPrefixParser(p.currentToken.Type)
		return nil
	}

	leftExpression := prefixParserFn()

	// Step 2: Keep parsing infix operators while they have higher precedence
	for p.nextToken.Type != token.SEMICOLON && minimumPrecedence < p.getPrecedenceOfNextToken() {
		infixParserFn := p.infixParsers[p.nextToken.Type]
		if infixParserFn == nil {
			return leftExpression
		}

		p.advanceToNextToken()
		leftExpression = infixParserFn(leftExpression)
	}

	return leftExpression
}

// parseIdentifier parses a variable or function name.
// Example: x, myVariable, calculateSum
func (p *Parser) parseIdentifier() ast.Expression {
	return p.buildIdentifierFromCurrentToken()
}

// parseIntegerLiteral parses a numeric literal.
// Example: 5, 42, 1000
func (p *Parser) parseIntegerLiteral() ast.Expression {
	literalToken := p.currentToken

	numericValue, parseError := strconv.ParseInt(literalToken.Literal, 0, 64)
	if parseError != nil {
		p.reportError("Failed to parse '%s' as an integer", literalToken.Literal)
		return nil
	}

	return &ast.IntegerLiteral{
		Token: literalToken,
		Value: numericValue,
	}
}

// parseBooleanLiteral parses boolean values.
// Example: true, false
func (p *Parser) parseBooleanLiteral() ast.Expression {
	return &ast.Boolean{
		Token: p.currentToken,
		Value: p.currentToken.Type == token.TRUE,
	}
}

// parsePrefixOperatorExpression parses unary operators before an expression.
// Format: <operator><expression>
// Example: -5, !true, -x
func (p *Parser) parsePrefixOperatorExpression() ast.Expression {
	operatorToken := p.currentToken
	operatorSymbol := operatorToken.Literal

	p.advanceToNextToken()
	rightSideExpression := p.parseExpression(PREFIX)

	return &ast.PrefixExpression{
		Token:    operatorToken,
		Operator: operatorSymbol,
		Right:    rightSideExpression,
	}
}

// parseBinaryOperatorExpression parses operators between two expressions.
// Format: <left> <operator> <right>
// Example: 3 + 5, x * y, a == b
func (p *Parser) parseBinaryOperatorExpression(leftSideExpression ast.Expression) ast.Expression {
	operatorToken := p.currentToken
	operatorSymbol := operatorToken.Literal
	operatorPrecedence := p.getPrecedenceOfCurrentToken()

	p.advanceToNextToken()
	rightSideExpression := p.parseExpression(operatorPrecedence)

	return &ast.InfixExpression{
		Token:    operatorToken,
		Operator: operatorSymbol,
		Left:     leftSideExpression,
		Right:    rightSideExpression,
	}
}

// parseGroupedExpression parses expressions wrapped in parentheses.
// Format: (<expression>)
// Example: (5 + 3), (x * y + z)
func (p *Parser) parseGroupedExpression() ast.Expression {
	p.advanceToNextToken() // Skip opening '('

	innerExpression := p.parseExpression(LOWEST)

	if !p.expectNextTokenToBe(token.RPAREN) {
		return nil
	}

	return innerExpression
}

// ============================================================================
// COMPLEX EXPRESSION PARSING
// ============================================================================

// parseIfExpression parses conditional expressions.
// Format: if (<condition>) { <consequence> } else { <alternative> }
// Example: if (x > 5) { return x; } else { return 0; }
func (p *Parser) parseIfExpression() ast.Expression {
	ifToken := p.currentToken

	// Parse: if (condition)
	if !p.expectNextTokenToBe(token.LPAREN) {
		return nil
	}

	p.advanceToNextToken()
	conditionExpression := p.parseExpression(LOWEST)

	if !p.expectNextTokenToBe(token.RPAREN) {
		return nil
	}

	// Parse: { consequence }
	if !p.expectNextTokenToBe(token.LBRACE) {
		return nil
	}

	consequenceBlock := p.parseBlockStatement()

	// Parse optional: else { alternative }
	var alternativeBlock *ast.BlockStatement
	if p.nextToken.Type == token.ELSE {
		p.advanceToNextToken()

		if !p.expectNextTokenToBe(token.LBRACE) {
			return nil
		}

		alternativeBlock = p.parseBlockStatement()
	}

	return &ast.IfExpression{
		Token:       ifToken,
		Condition:   conditionExpression,
		Consequence: consequenceBlock,
		Alternative: alternativeBlock,
	}
}

// parseBlockStatement parses a sequence of statements within braces.
// Format: { <statement>; <statement>; ... }
// Example: { let x = 5; return x; }
func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	blockToken := p.currentToken
	statements := []ast.Statement{}

	p.advanceToNextToken()

	for p.currentToken.Type != token.RBRACE && p.currentToken.Type != token.EOF {
		statement := p.parseStatement()
		if statement != nil {
			statements = append(statements, statement)
		}
		p.advanceToNextToken()
	}

	return &ast.BlockStatement{
		Token:      blockToken,
		Statements: statements,
	}
}

// parseFunctionLiteral parses function definitions.
// Format: fn(<parameters>) { <body> }
// Example: fn(x, y) { return x + y; }
func (p *Parser) parseFunctionLiteral() ast.Expression {
	functionToken := p.currentToken

	if !p.expectNextTokenToBe(token.LPAREN) {
		return nil
	}

	parameterList := p.parseCommaSeparatedIdentifiers(token.RPAREN)
	if parameterList == nil {
		return nil
	}

	if !p.expectNextTokenToBe(token.LBRACE) {
		return nil
	}

	functionBody := p.parseBlockStatement()

	return &ast.FunctionLiteral{
		Token:      functionToken,
		Parameters: parameterList,
		Body:       functionBody,
	}
}

// parseFunctionCallExpression parses function invocations.
// Format: <function>(<arguments>)
// Example: add(2, 3), multiply(x, y, z)
func (p *Parser) parseFunctionCallExpression(functionExpression ast.Expression) ast.Expression {
	callToken := p.currentToken
	argumentList := p.parseCommaSeparatedExpressions(token.RPAREN)

	return &ast.CallExpression{
		Token:     callToken,
		Function:  functionExpression,
		Arguments: argumentList,
	}
}

// ============================================================================
// PARSING COMMA-SEPARATED LISTS
// ============================================================================

// parseCommaSeparatedIdentifiers parses a list of identifiers separated by commas.
// Format: () or (x) or (x, y, z)
// Used for function parameters.
func (p *Parser) parseCommaSeparatedIdentifiers(closingToken token.TokenType) []*ast.Identifier {
	identifierList := []*ast.Identifier{}

	// Handle empty list: ()
	if p.nextToken.Type == closingToken {
		p.advanceToNextToken()
		return identifierList
	}

	// Parse first identifier
	p.advanceToNextToken()
	identifierList = append(identifierList, p.buildIdentifierFromCurrentToken())

	// Parse remaining identifiers after commas
	for p.nextToken.Type == token.COMMA {
		p.advanceToNextToken() // Move past comma
		p.advanceToNextToken() // Move to next identifier
		identifierList = append(identifierList, p.buildIdentifierFromCurrentToken())
	}

	if !p.expectNextTokenToBe(closingToken) {
		return nil
	}

	return identifierList
}

// parseCommaSeparatedExpressions parses a list of expressions separated by commas.
// Format: () or (5) or (x, 2 + 2, y * 3)
// Used for function arguments.
func (p *Parser) parseCommaSeparatedExpressions(closingToken token.TokenType) []ast.Expression {
	expressionList := []ast.Expression{}

	// Handle empty list: ()
	if p.nextToken.Type == closingToken {
		p.advanceToNextToken()
		return expressionList
	}

	// Parse first expression
	p.advanceToNextToken()
	expressionList = append(expressionList, p.parseExpression(LOWEST))

	// Parse remaining expressions after commas
	for p.nextToken.Type == token.COMMA {
		p.advanceToNextToken() // Move past comma
		p.advanceToNextToken() // Move to next expression
		expressionList = append(expressionList, p.parseExpression(LOWEST))
	}

	if !p.expectNextTokenToBe(closingToken) {
		return nil
	}

	return expressionList
}

// ============================================================================
// TOKEN NAVIGATION HELPERS
// ============================================================================

// advanceToNextToken moves forward one token in the stream.
func (p *Parser) advanceToNextToken() {
	p.currentToken = p.nextToken
	p.nextToken = p.lexer.NextToken()
}

// expectNextTokenToBe checks if the next token matches what we expect.
// If it does, we advance to it and return true.
// If it doesn't, we record an error and return false.
func (p *Parser) expectNextTokenToBe(expectedType token.TokenType) bool {
	if p.nextToken.Type == expectedType {
		p.advanceToNextToken()
		return true
	}

	p.reportUnexpectedToken(expectedType)
	return false
}

// skipSemicolonIfPresent advances past a semicolon if one is present.
// Semicolons are optional in our language.
func (p *Parser) skipSemicolonIfPresent() {
	if p.nextToken.Type == token.SEMICOLON {
		p.advanceToNextToken()
	}
}

// ============================================================================
// PRECEDENCE HELPERS
// ============================================================================

// getPrecedenceOfCurrentToken returns how tightly the current token binds.
func (p *Parser) getPrecedenceOfCurrentToken() int {
	if precedenceLevel, found := precedenceTable[p.currentToken.Type]; found {
		return precedenceLevel
	}
	return LOWEST
}

// getPrecedenceOfNextToken returns how tightly the next token binds.
func (p *Parser) getPrecedenceOfNextToken() int {
	if precedenceLevel, found := precedenceTable[p.nextToken.Type]; found {
		return precedenceLevel
	}
	return LOWEST
}

// ============================================================================
// AST NODE BUILDERS
// ============================================================================

// buildIdentifierFromCurrentToken creates an Identifier AST node from the current token.
func (p *Parser) buildIdentifierFromCurrentToken() *ast.Identifier {
	return &ast.Identifier{
		Token: p.currentToken,
		Value: p.currentToken.Literal,
	}
}

// ============================================================================
// ERROR REPORTING
// ============================================================================

// reportError adds a formatted error message to the error list.
func (p *Parser) reportError(messageFormat string, arguments ...interface{}) {
	errorMessage := fmt.Sprintf(messageFormat, arguments...)
	p.errors = append(p.errors, errorMessage)
}

// reportUnexpectedToken records an error when we expected one token but got another.
func (p *Parser) reportUnexpectedToken(expectedType token.TokenType) {
	p.reportError(
		"Expected next token to be '%s', but got '%s' instead",
		expectedType,
		p.nextToken.Type,
	)
}

// reportMissingPrefixParser records an error when we can't parse a token at the start of an expression.
func (p *Parser) reportMissingPrefixParser(tokenType token.TokenType) {
	p.reportError(
		"No parser registered for token '%s' at the start of an expression",
		tokenType,
	)
}
