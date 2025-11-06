package parser_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/ucok-man/froggy/internal/ast"
	"github.com/ucok-man/froggy/internal/lexer"
	"github.com/ucok-man/froggy/internal/parser"
)

func TestLetStatements(t *testing.T) {
	tests := []struct {
		name          string
		input         string
		expectedIdent string
		expectedValue any
	}{
		{
			name:          "integer value",
			input:         "let x = 5;",
			expectedIdent: "x",
			expectedValue: 5,
		},
		{
			name:          "boolean value",
			input:         "let y = true;",
			expectedIdent: "y",
			expectedValue: true,
		},
		{
			name:          "identifier value",
			input:         "let foobar = y;",
			expectedIdent: "foobar",
			expectedValue: "y",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			program := parseProgram(t, tt.input)

			assertStatementCount(t, program, 1)

			stmt := program.Statements[0]
			assertLetStatement(t, stmt, tt.expectedIdent)

			val := stmt.(*ast.LetStatement).Value
			assertLiteralExpression(t, val, tt.expectedValue)
		})
	}
}

func TestReturnStatements(t *testing.T) {
	tests := []struct {
		name          string
		input         string
		expectedValue any
	}{
		{
			name:          "return integer",
			input:         "return 5;",
			expectedValue: 5,
		},
		{
			name:          "return boolean",
			input:         "return true;",
			expectedValue: true,
		},
		{
			name:          "return identifier",
			input:         "return foobar;",
			expectedValue: "foobar",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			program := parseProgram(t, tt.input)

			assertStatementCount(t, program, 1)

			returnStmt := assertReturnStatement(t, program.Statements[0])
			assertLiteralExpression(t, returnStmt.ReturnValue, tt.expectedValue)
		})
	}
}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"
	program := parseProgram(t, input)

	assertStatementCount(t, program, 1)

	stmt := assertExpressionStatement(t, program.Statements[0])
	ident := assertIdentifier(t, stmt.Expression, "foobar")

	assertEqual(t, "astnode.Value", "foobar", ident.Value)
	assertEqual(t, "astnode.TokenLiteral", "foobar", ident.TokenLiteral())
}

func TestIntegerLiteralExpression(t *testing.T) {
	input := "5;"
	program := parseProgram(t, input)

	assertStatementCount(t, program, 1)

	stmt := assertExpressionStatement(t, program.Statements[0])
	literal := assertIntegerLiteral(t, stmt.Expression, 5)

	assertEqual(t, "literal.TokenLiteral", "5", literal.TokenLiteral())
}

func TestParsingPrefixExpressions(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		operator string
		value    any
	}{
		{"bang integer", "!5;", "!", 5},
		{"minus integer", "-15;", "-", 15},
		{"bang identifier", "!foobar;", "!", "foobar"},
		{"minus identifier", "-foobar;", "-", "foobar"},
		{"bang true", "!true;", "!", true},
		{"bang false", "!false;", "!", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			program := parseProgram(t, tt.input)

			assertStatementCount(t, program, 1)

			stmt := assertExpressionStatement(t, program.Statements[0])
			exp := assertPrefixExpression(t, stmt.Expression)

			assertEqual(t, "exp.Operator", tt.operator, exp.Operator)
			assertLiteralExpression(t, exp.Right, tt.value)
		})
	}
}

func TestParsingInfixExpressions(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		leftValue  any
		operator   string
		rightValue any
	}{
		{"addition", "5 + 5;", 5, "+", 5},
		{"subtraction", "5 - 5;", 5, "-", 5},
		{"multiplication", "5 * 5;", 5, "*", 5},
		{"division", "5 / 5;", 5, "/", 5},
		{"greater than", "5 > 5;", 5, ">", 5},
		{"less than", "5 < 5;", 5, "<", 5},
		{"equality", "5 == 5;", 5, "==", 5},
		{"inequality", "5 != 5;", 5, "!=", 5},
		{"identifier addition", "foobar + barfoo;", "foobar", "+", "barfoo"},
		{"identifier subtraction", "foobar - barfoo;", "foobar", "-", "barfoo"},
		{"boolean equality", "true == true", true, "==", true},
		{"boolean inequality", "true != false", true, "!=", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			program := parseProgram(t, tt.input)

			assertStatementCount(t, program, 1)

			stmt := assertExpressionStatement(t, program.Statements[0])
			assertInfixExpression(t, stmt.Expression, tt.leftValue, tt.operator, tt.rightValue)
		})
	}
}

func TestOperatorPrecedenceParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"-a * b", "((-a) * b)"},
		{"!-a", "(!(-a))"},
		{"a + b + c", "((a + b) + c)"},
		{"a + b - c", "((a + b) - c)"},
		{"a * b * c", "((a * b) * c)"},
		{"a * b / c", "((a * b) / c)"},
		{"a + b / c", "(a + (b / c))"},
		{"a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"},
		{"3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"},
		{"5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"},
		{"5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"},
		{"3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
		{"true", "true"},
		{"false", "false"},
		{"3 > 5 == false", "((3 > 5) == false)"},
		{"3 < 5 == true", "((3 < 5) == true)"},
		{"1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"},
		{"(5 + 5) * 2", "((5 + 5) * 2)"},
		{"2 / (5 + 5)", "(2 / (5 + 5))"},
		{"(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"},
		{"-(5 + 5)", "(-(5 + 5))"},
		{"!(true == true)", "(!(true == true))"},
		{"a + add(b * c) + d", "((a + add((b * c))) + d)"},
		{"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"},
		{"add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			program := parseProgram(t, tt.input)

			actual := program.String()
			assertEqual(t, "precedence", tt.expected, actual)
		})
	}
}

func TestBooleanExpression(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected bool
	}{
		{"true literal", "true;", true},
		{"false literal", "false;", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			program := parseProgram(t, tt.input)

			assertStatementCount(t, program, 1)

			stmt := assertExpressionStatement(t, program.Statements[0])
			boolean := assertBoolean(t, stmt.Expression)

			assertEqual(t, "boolean.Value", tt.expected, boolean.Value)
		})
	}
}

func TestIfExpression(t *testing.T) {
	input := `if (x < y) { x }`
	program := parseProgram(t, input)

	assertStatementCount(t, program, 1)

	stmt := assertExpressionStatement(t, program.Statements[0])
	exp := assertIfExpression(t, stmt.Expression)

	assertInfixExpression(t, exp.Condition, "x", "<", "y")
	assertStatementCount(t, exp.Consequence, 1)

	consequence := assertExpressionStatement(t, exp.Consequence.Statements[0])
	assertIdentifier(t, consequence.Expression, "x")

	assertNil(t, "exp.Alternative", exp.Alternative)
}

func TestIfElseExpression(t *testing.T) {
	input := `if (x < y) { x } else { y }`
	program := parseProgram(t, input)

	assertStatementCount(t, program, 1)

	stmt := assertExpressionStatement(t, program.Statements[0])
	exp := assertIfExpression(t, stmt.Expression)

	assertInfixExpression(t, exp.Condition, "x", "<", "y")

	assertStatementCount(t, exp.Consequence, 1)
	consequence := assertExpressionStatement(t, exp.Consequence.Statements[0])
	assertIdentifier(t, consequence.Expression, "x")

	assertStatementCount(t, exp.Alternative, 1)
	alternative := assertExpressionStatement(t, exp.Alternative.Statements[0])
	assertIdentifier(t, alternative.Expression, "y")
}

func TestFunctionLiteralParsing(t *testing.T) {
	input := `fn(x, y) { x + y; }`
	program := parseProgram(t, input)

	assertStatementCount(t, program, 1)

	stmt := assertExpressionStatement(t, program.Statements[0])
	function := assertFunctionLiteral(t, stmt.Expression)

	assertParameterCount(t, function, 2)
	assertLiteralExpression(t, function.Parameters[0], "x")
	assertLiteralExpression(t, function.Parameters[1], "y")

	assertStatementCount(t, function.Body, 1)
	bodyStmt := assertExpressionStatement(t, function.Body.Statements[0])
	assertInfixExpression(t, bodyStmt.Expression, "x", "+", "y")
}

func TestFunctionParameterParsing(t *testing.T) {
	tests := []struct {
		name           string
		input          string
		expectedParams []string
	}{
		{"no parameters", "fn() {};", []string{}},
		{"one parameter", "fn(x) {};", []string{"x"}},
		{"multiple parameters", "fn(x, y, z) {};", []string{"x", "y", "z"}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			program := parseProgram(t, tt.input)

			stmt := assertExpressionStatement(t, program.Statements[0])
			function := assertFunctionLiteral(t, stmt.Expression)

			assertParameterCount(t, function, len(tt.expectedParams))

			for i, param := range tt.expectedParams {
				assertLiteralExpression(t, function.Parameters[i], param)
			}
		})
	}
}

func TestCallExpressionParsing(t *testing.T) {
	input := "add(1, 2 * 3, 4 + 5);"
	program := parseProgram(t, input)

	assertStatementCount(t, program, 1)

	stmt := assertExpressionStatement(t, program.Statements[0])
	exp := assertCallExpression(t, stmt.Expression)

	assertIdentifier(t, exp.Function, "add")
	assertArgumentCount(t, exp, 3)

	assertLiteralExpression(t, exp.Arguments[0], 1)
	assertInfixExpression(t, exp.Arguments[1], 2, "*", 3)
	assertInfixExpression(t, exp.Arguments[2], 4, "+", 5)
}

func TestCallExpressionParameterParsing(t *testing.T) {
	tests := []struct {
		name          string
		input         string
		expectedIdent string
		expectedArgs  []string
	}{
		{"no arguments", "add();", "add", []string{}},
		{"one argument", "add(1);", "add", []string{"1"}},
		{"multiple arguments", "add(1, 2 * 3, 4 + 5);", "add", []string{"1", "(2 * 3)", "(4 + 5)"}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			program := parseProgram(t, tt.input)

			stmt := assertExpressionStatement(t, program.Statements[0])
			exp := assertCallExpression(t, stmt.Expression)

			assertIdentifier(t, exp.Function, tt.expectedIdent)
			assertArgumentCount(t, exp, len(tt.expectedArgs))

			for i, arg := range tt.expectedArgs {
				assertEqual(t, fmt.Sprintf("argument %d", i), arg, exp.Arguments[i].String())
			}
		})
	}
}

// ============================================================================
// Helper Functions
// ============================================================================

func parseProgram(t *testing.T, input string) *ast.Program {
	t.Helper()

	l := lexer.New(input)
	p := parser.New(l)
	program := p.ParseProgram()

	checkParserErrors(t, p)

	return program
}

func checkParserErrors(t *testing.T, p *parser.Parser) {
	t.Helper()

	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}

// ============================================================================
// Assertion Helpers
// ============================================================================

func assertEqual(t *testing.T, name string, expected, actual any) {
	t.Helper()
	assert.Equal(t, expected, actual, fmt.Sprintf("%s: expected=%v, got=%v", name, expected, actual))

}

func assertNil(t *testing.T, name string, value any) {
	t.Helper()
	assert.Nil(t, value, fmt.Sprintf("%s should be nil, got=%+v", name, value))
}

func assertStatementCount(t *testing.T, container any, expected int) {
	t.Helper()

	var statements []ast.Statement
	switch v := container.(type) {
	case *ast.Program:
		statements = v.Statements
	case *ast.BlockStatement:
		statements = v.Statements
	}

	if len(statements) != expected {
		t.Fatalf("expected %d statements, got=%d", expected, len(statements))
	}
}

func assertParameterCount(t *testing.T, fn *ast.FunctionLiteral, expected int) {
	t.Helper()
	if len(fn.Parameters) != expected {
		t.Fatalf("expected %d parameters, got=%d", expected, len(fn.Parameters))
	}
}

func assertArgumentCount(t *testing.T, call *ast.CallExpression, expected int) {
	t.Helper()
	if len(call.Arguments) != expected {
		t.Fatalf("expected %d arguments, got=%d", expected, len(call.Arguments))
	}
}

func assertLetStatement(t *testing.T, s ast.Statement, name string) *ast.LetStatement {
	t.Helper()

	assertEqual(t, "TokenLiteral", "let", s.TokenLiteral())

	letStmt, ok := s.(*ast.LetStatement)
	if !ok {
		t.Fatalf("statement is not *ast.LetStatement, got=%T", s)
	}

	assertEqual(t, "Identifier.Value", name, letStmt.Identifier.Value)
	assertEqual(t, "Identifier.TokenLiteral", name, letStmt.Identifier.TokenLiteral())

	return letStmt
}

func assertReturnStatement(t *testing.T, s ast.Statement) *ast.ReturnStatement {
	t.Helper()

	returnStmt, ok := s.(*ast.ReturnStatement)
	if !ok {
		t.Fatalf("statement is not *ast.ReturnStatement, got=%T", s)
	}

	assertEqual(t, "TokenLiteral", "return", returnStmt.TokenLiteral())

	return returnStmt
}

func assertExpressionStatement(t *testing.T, s ast.Statement) *ast.ExpressionStatement {
	t.Helper()

	stmt, ok := s.(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("statement is not *ast.ExpressionStatement, got=%T", s)
	}

	return stmt
}

func assertPrefixExpression(t *testing.T, exp ast.Expression) *ast.PrefixExpression {
	t.Helper()

	prefixExp, ok := exp.(*ast.PrefixExpression)
	if !ok {
		t.Fatalf("expression is not *ast.PrefixExpression, got=%T", exp)
	}

	return prefixExp
}

func assertIfExpression(t *testing.T, exp ast.Expression) *ast.IfExpression {
	t.Helper()

	ifExp, ok := exp.(*ast.IfExpression)
	if !ok {
		t.Fatalf("expression is not *ast.IfExpression, got=%T", exp)
	}

	return ifExp
}

func assertFunctionLiteral(t *testing.T, exp ast.Expression) *ast.FunctionLiteral {
	t.Helper()

	fn, ok := exp.(*ast.FunctionLiteral)
	if !ok {
		t.Fatalf("expression is not *ast.FunctionLiteral, got=%T", exp)
	}

	return fn
}

func assertCallExpression(t *testing.T, exp ast.Expression) *ast.CallExpression {
	t.Helper()

	call, ok := exp.(*ast.CallExpression)
	if !ok {
		t.Fatalf("expression is not *ast.CallExpression, got=%T", exp)
	}

	return call
}

func assertInfixExpression(t *testing.T, exp ast.Expression, left any, operator string, right any) {
	t.Helper()

	infixExp, ok := exp.(*ast.InfixExpression)
	if !ok {
		t.Fatalf("expression is not *ast.InfixExpression, got=%T(%s)", exp, exp)
	}

	assertLiteralExpression(t, infixExp.Left, left)
	assertEqual(t, "Operator", operator, infixExp.Operator)
	assertLiteralExpression(t, infixExp.Right, right)
}

func assertLiteralExpression(t *testing.T, exp ast.Expression, expected any) {
	t.Helper()

	switch v := expected.(type) {
	case int:
		assertIntegerLiteral(t, exp, int64(v))
	case int64:
		assertIntegerLiteral(t, exp, v)
	case string:
		assertIdentifier(t, exp, v)
	case bool:
		assertBooleanLiteral(t, exp, v)
	default:
		t.Fatalf("type of expression not handled, got=%T", exp)
	}
}

func assertIntegerLiteral(t *testing.T, exp ast.Expression, value int64) *ast.IntegerLiteral {
	t.Helper()

	literal, ok := exp.(*ast.IntegerLiteral)
	if !ok {
		t.Fatalf("expression is not *ast.IntegerLiteral, got=%T", exp)
	}

	assertEqual(t, "IntegerLiteral.Value", value, literal.Value)
	assertEqual(t, "IntegerLiteral.TokenLiteral", fmt.Sprintf("%d", value), literal.TokenLiteral())

	return literal
}

func assertIdentifier(t *testing.T, exp ast.Expression, value string) *ast.Identifier {
	t.Helper()

	ident, ok := exp.(*ast.Identifier)
	if !ok {
		t.Fatalf("expression is not *ast.Identifier, got=%T", exp)
	}

	assertEqual(t, "Identifier.Value", value, ident.Value)
	assertEqual(t, "Identifier.TokenLiteral", value, ident.TokenLiteral())

	return ident
}

func assertBooleanLiteral(t *testing.T, exp ast.Expression, value bool) *ast.Boolean {
	t.Helper()

	boolean, ok := exp.(*ast.Boolean)
	if !ok {
		t.Fatalf("expression is not *ast.Boolean, got=%T", exp)
	}

	assertEqual(t, "Boolean.Value", value, boolean.Value)
	assertEqual(t, "Boolean.TokenLiteral", fmt.Sprintf("%t", value), boolean.TokenLiteral())

	return boolean
}

func assertBoolean(t *testing.T, exp ast.Expression) *ast.Boolean {
	t.Helper()

	boolean, ok := exp.(*ast.Boolean)
	if !ok {
		t.Fatalf("expression is not *ast.Boolean, got=%T", exp)
	}

	return boolean
}
