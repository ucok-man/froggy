package parser

import (
	"fmt"

	"github.com/ucok-man/froggy/internal/ast"
	"github.com/ucok-man/froggy/internal/lexer"
	"github.com/ucok-man/froggy/internal/token"
)

type Parser struct {
	l            *lexer.Lexer
	currentToken token.Token
	peekToken    token.Token
	errors       []string
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: make([]string, 0),
	}
	// Baca dua token pertama, sehingga curToken dan peekToken terisi
	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{
		Statements: make([]ast.Statement, 0),
	}

	for p.currentToken.Type != token.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}
	return program
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) nextToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) currentTokenIs(t token.TokenType) bool {
	return p.currentToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf(
		"expected next token to be %s, got %s instead",
		t, p.peekToken.Type,
	)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.currentToken.Type {
	case token.LET:
		return p.parseLetStatement()
	default:
		return nil
	}
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{
		Token: p.currentToken,
	}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{
		Token: p.currentToken,
		Value: p.currentToken.Literal,
	}

	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	// TODO: We're skipping the expressions until we
	// encounter a semicolon
	for !p.currentTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}
