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

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

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
	}

	l.readChar()
	return tok
}
