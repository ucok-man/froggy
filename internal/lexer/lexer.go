// Package lexer implements the lexical analysis (tokenization) phase for the Froggy programming language.
// It converts raw source code input into a stream of tokens for the parser.
package lexer

import (
	"github.com/ucok-man/froggy/internal/token"
)

// ============================================================================
// Lexer Structure
// ============================================================================

// Lexer performs lexical analysis on input source code.
// It maintains the current position in the input and produces tokens on demand.
type Lexer struct {
	input        string // the source code being tokenized
	position     int    // current position in input (points to current char)
	readPosition int    // current reading position in input (after current char)
	currentChar  byte   // current character under examination
}

// New creates and initializes a new Lexer for the given input string.
// It positions the lexer at the first character of the input.
func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

// ============================================================================
// Character Classification Helpers
// ============================================================================

// isLetter returns true if the given character is a valid letter for identifiers.
// Valid identifier characters are: a-z, A-Z, and underscore (_).
func isLetter(char byte) bool {
	return 'a' <= char && char <= 'z' || 'A' <= char && char <= 'Z' || char == '_'
}

// isDigit returns true if the given character is a numeric digit (0-9).
func isDigit(char byte) bool {
	return '0' <= char && char <= '9'
}

// ============================================================================
// Character Navigation
// ============================================================================

// readChar advances the lexer to the next character in the input.
// When the end of input is reached, it sets currentChar to 0 (ASCII NUL/EOF).
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.currentChar = 0 // 0 represents EOF in ASCII
	} else {
		l.currentChar = l.input[l.readPosition]
	}

	l.position = l.readPosition
	l.readPosition++
}

// peekChar returns the next character in the input without advancing the lexer position.
// Returns 0 (EOF) if at the end of input.
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

// skipWhitespace advances the lexer past all whitespace characters.
// Whitespace includes spaces, tabs, newlines (\n), and carriage returns (\r).
func (l *Lexer) skipWhitespace() {
	for l.currentChar == ' ' || l.currentChar == '\t' || l.currentChar == '\n' || l.currentChar == '\r' {
		l.readChar()
	}
}

// ============================================================================
// Token Reading Functions
// ============================================================================

// readIdentifier reads and returns an identifier from the current position.
// An identifier consists of letters (a-z, A-Z), underscores, continuing until
// a non-letter character is encountered.
func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.currentChar) {
		l.readChar()
	}
	return l.input[position:l.position]
}

// readNumber reads and returns a number literal from the current position.
// A number consists of consecutive digits (0-9).
// Note: Currently only supports integers, not floating-point numbers.
func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.currentChar) {
		l.readChar()
	}
	return l.input[position:l.position]
}

// ============================================================================
// Main Tokenization Function
// ============================================================================

// NextToken scans and returns the next token from the input.
// It skips whitespace and recognizes:
//   - Operators: =, ==, !, !=, +, -, /, *, <, >
//   - Delimiters: ;, ,, (, ), {, }
//   - Keywords and identifiers: let, fn, if, else, return, true, false
//   - Integer literals
//   - EOF (end of file)
//   - ILLEGAL tokens for unrecognized characters
func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace()

	switch l.currentChar {
	case '=':
		if l.peekChar() == '=' {
			// Two-character token: ==
			char := l.currentChar
			l.readChar()
			literal := string(char) + string(l.currentChar)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else {
			tok = token.New(token.ASSIGN, l.currentChar)
		}
	case '!':
		if l.peekChar() == '=' {
			// Two-character token: !=
			char := l.currentChar
			l.readChar()
			literal := string(char) + string(l.currentChar)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else {
			tok = token.New(token.BANG, l.currentChar)
		}
	case '+':
		tok = token.New(token.PLUS, l.currentChar)
	case '-':
		tok = token.New(token.MINUS, l.currentChar)
	case '/':
		tok = token.New(token.SLASH, l.currentChar)
	case '*':
		tok = token.New(token.ASTERISK, l.currentChar)
	case '<':
		tok = token.New(token.LT, l.currentChar)
	case '>':
		tok = token.New(token.GT, l.currentChar)
	case ';':
		tok = token.New(token.SEMICOLON, l.currentChar)
	case ',':
		tok = token.New(token.COMMA, l.currentChar)
	case '(':
		tok = token.New(token.LPAREN, l.currentChar)
	case ')':
		tok = token.New(token.RPAREN, l.currentChar)
	case '{':
		tok = token.New(token.LBRACE, l.currentChar)
	case '}':
		tok = token.New(token.RBRACE, l.currentChar)
	case 0:
		// End of file
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.currentChar) {
			// Read identifier or keyword
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			return tok // Early return because readIdentifier already advanced
		} else if isDigit(l.currentChar) {
			// Read integer literal
			tok.Type = token.INT
			tok.Literal = l.readNumber()
			return tok // Early return because readNumber already advanced
		} else {
			// Unrecognized character
			tok = token.New(token.ILLEGAL, l.currentChar)
		}
	}

	l.readChar()
	return tok
}
