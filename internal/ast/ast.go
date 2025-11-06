// Package ast defines the Abstract Syntax Tree (AST) structure for the Froggy programming language.
// It provides node types for representing program structure, statements, and expressions.
package ast

import (
	"bytes"
	"strings"

	"github.com/ucok-man/froggy/internal/token"
)

// ============================================================================
// Base Interfaces
// ============================================================================

// Node is the base interface that all AST nodes must implement.
// It provides methods for getting the token literal and string representation.
type Node interface {
	TokenLiteral() string // Returns the literal value of the token
	String() string       // Returns a string representation for debugging
}

// Statement represents any statement node in the AST.
// Statements perform actions but do not produce values.
type Statement interface {
	Node
	statementNode()
}

// Expression represents any expression node in the AST.
// Expressions produce values when evaluated.
type Expression interface {
	Node
	expressionNode()
}

// ============================================================================
// Program Root
// ============================================================================

// Program is the root node of every AST produced by the parser.
// It contains a slice of statements that make up the entire program.
type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string { return "" }

func (p *Program) String() string {
	var out bytes.Buffer
	for _, s := range p.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}

// ============================================================================
// Statement Nodes
// ============================================================================

// LetStatement represents a variable binding statement.
// Syntax: let <identifier> = <expression>;
type LetStatement struct {
	Token      token.Token // the token.LET token
	Identifier *Identifier // the identifier being bound
	Value      Expression  // the expression producing the value
}

func (ls *LetStatement) statementNode()       {}
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

func (ls *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Identifier.String())
	out.WriteString(" = ")

	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}

	out.WriteString(";")
	return out.String()
}

// ----------------------------------------------------------------------------

// ReturnStatement represents a return statement.
// Syntax: return <expression>;
type ReturnStatement struct {
	Token       token.Token // the 'return' token
	ReturnValue Expression  // the expression to return
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }

func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(rs.TokenLiteral() + " ")

	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}

	out.WriteString(";")
	return out.String()
}

// ----------------------------------------------------------------------------

// ExpressionStatement wraps an expression to allow it to be used as a statement.
// This is used for expressions that are evaluated for their side effects.
type ExpressionStatement struct {
	Token      token.Token // the first token of the expression
	Expression Expression  // the expression being evaluated
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }

func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// ----------------------------------------------------------------------------

// BlockStatement represents a block of statements enclosed in braces.
// Syntax: { <statement>* }
type BlockStatement struct {
	Token      token.Token // the { token
	Statements []Statement // statements within the block
}

func (bs *BlockStatement) statementNode()       {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }

func (bs *BlockStatement) String() string {
	var out bytes.Buffer
	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}

// ============================================================================
// Literal Expression Nodes
// ============================================================================

// Identifier represents a variable or function name.
type Identifier struct {
	Token token.Token // the token.IDENT token
	Value string      // the actual identifier name
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string       { return i.Value }

// ----------------------------------------------------------------------------

// Boolean represents a boolean literal (true or false).
type Boolean struct {
	Token token.Token // the token (TRUE or FALSE)
	Value bool        // the boolean value
}

func (b *Boolean) expressionNode()      {}
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }
func (b *Boolean) String() string       { return b.Token.Literal }

// ----------------------------------------------------------------------------

// IntegerLiteral represents an integer number literal.
type IntegerLiteral struct {
	Token token.Token // the token containing the integer
	Value int64       // the parsed integer value
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

// ----------------------------------------------------------------------------

// FunctionLiteral represents a function definition.
// Syntax: fn(<parameter>, <parameter>, ...) { <block> }
type FunctionLiteral struct {
	Token      token.Token     // the 'fn' token
	Parameters []*Identifier   // function parameters
	Body       *BlockStatement // function body
}

func (fl *FunctionLiteral) expressionNode()      {}
func (fl *FunctionLiteral) TokenLiteral() string { return fl.Token.Literal }

func (fl *FunctionLiteral) String() string {
	var out bytes.Buffer

	params := []string{}
	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}

	out.WriteString(fl.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(fl.Body.String())

	return out.String()
}

// ============================================================================
// Operator Expression Nodes
// ============================================================================

// PrefixExpression represents a prefix operator expression.
// Syntax: <operator><expression>
// Examples: -5, !true
type PrefixExpression struct {
	Token    token.Token // the prefix token, e.g. !, -
	Operator string      // the operator as a string
	Right    Expression  // the right-hand side expression
}

func (pe *PrefixExpression) expressionNode()      {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }

func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")

	return out.String()
}

// ----------------------------------------------------------------------------

// InfixExpression represents an infix (binary) operator expression.
// Syntax: <expression> <operator> <expression>
// Examples: 5 + 5, x * y, a == b
type InfixExpression struct {
	Token    token.Token // the operator token, e.g. +, -, *, /
	Left     Expression  // the left-hand side expression
	Operator string      // the operator as a string
	Right    Expression  // the right-hand side expression
}

func (ie *InfixExpression) expressionNode()      {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }

func (ie *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")

	return out.String()
}

// ============================================================================
// Control Flow Expression Nodes
// ============================================================================

// IfExpression represents a conditional if-else expression.
// Syntax: if (<condition>) { <consequence> } else { <alternative> }
// The else clause is optional.
type IfExpression struct {
	Token       token.Token     // the 'if' token
	Condition   Expression      // the condition to evaluate
	Consequence *BlockStatement // block to execute if condition is true
	Alternative *BlockStatement // optional block to execute if condition is false
}

func (ie *IfExpression) expressionNode()      {}
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }

func (ie *IfExpression) String() string {
	var out bytes.Buffer

	out.WriteString("if")
	out.WriteString(ie.Condition.String())
	out.WriteString(" ")
	out.WriteString(ie.Consequence.String())

	if ie.Alternative != nil {
		out.WriteString("else ")
		out.WriteString(ie.Alternative.String())
	}

	return out.String()
}

// ============================================================================
// Call Expression Node
// ============================================================================

// CallExpression represents a function call expression.
// Syntax: <function>(<argument>, <argument>, ...)
// The function can be an identifier or a function literal.
type CallExpression struct {
	Token     token.Token  // the '(' token
	Function  Expression   // the function being called (Identifier or FunctionLiteral)
	Arguments []Expression // arguments passed to the function
}

func (ce *CallExpression) expressionNode()      {}
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }

func (ce *CallExpression) String() string {
	var out bytes.Buffer

	args := []string{}
	for _, a := range ce.Arguments {
		args = append(args, a.String())
	}

	out.WriteString(ce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")

	return out.String()
}
