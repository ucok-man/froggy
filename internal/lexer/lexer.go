package lexer

import (
	"github.com/ucok-man/froggy/internal/token"
)

type Lexer struct {
	input        string
	position     int  // posisi saat ini dalam input (menunjuk ke karakter saat ini)
	readPosition int  // posisi baca berikutnya (setelah karakter saat ini)
	currentChar  byte // karakter yang sedang diperiksa
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.currentChar = 0 // -> 0 means EOF in ASCII
	} else {
		l.currentChar = l.input[l.readPosition]
	}

	l.position = l.readPosition
	l.readPosition++
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.currentChar) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func isLetter(char byte) bool {
	return 'a' <= char && char <= 'z' || 'A' <= char && char <= 'Z' || char == '_'
}

func (l *Lexer) skipWhitespace() {
	for l.currentChar == ' ' || l.currentChar == '\t' || l.currentChar == '\n' || l.currentChar == '\r' {
		l.readChar()
	}
}

func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.currentChar) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func isDigit(char byte) bool {
	return '0' <= char && char <= '9'
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace()

	switch l.currentChar {
	case '=':
		tok = token.New(token.ASSIGN, l.currentChar)
	case ';':
		tok = token.New(token.SEMICOLON, l.currentChar)
	case '(':
		tok = token.New(token.LPAREN, l.currentChar)
	case ')':
		tok = token.New(token.RPAREN, l.currentChar)
	case ',':
		tok = token.New(token.COMMA, l.currentChar)
	case '+':
		tok = token.New(token.PLUS, l.currentChar)
	case '{':
		tok = token.New(token.LBRACE, l.currentChar)
	case '}':
		tok = token.New(token.RBRACE, l.currentChar)
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.currentChar) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			return tok
		} else if isDigit(l.currentChar) {
			tok.Type = token.INT
			tok.Literal = l.readNumber()
			return tok
		} else {
			tok = token.New(token.ILLEGAL, l.currentChar)
		}
	}

	l.readChar()
	return tok
}
