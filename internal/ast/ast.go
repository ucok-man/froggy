package ast

import (
	"github.com/ucok-man/froggy/internal/token"
)

/* --------------------------- Defintion -------------------------- */

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

/* --------------------------- Root Node -------------------------- */
type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

/* ---------------------- Let Statement Node ---------------------- */

// Merepresentasikan let statement, misalnya: let x = 5;
type LetStatement struct {
	Token token.Token // token.LET
	Name  *Identifier // nama variabel (mis. "x")
	Value Expression  // expression di sisi kanan (mis. literal 5 atau lainnya)
}

func (ls *LetStatement) statementNode()       {}
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

/* ------------------------ Identifier Node ----------------------- */

// Merepresentasikan identifier (nama variabel)
type Identifier struct {
	Token token.Token // token.IDENT
	Value string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }

/* --------------------- Return Statement Node -------------------- */
type ReturnStatement struct {
	Token       token.Token // the 'return' token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
