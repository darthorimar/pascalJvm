package compiler.lexer

import scala.util.parsing.input.Positional

trait Token extends Positional

case class IDENTIFIER(name: String) extends Token
case class NUMBER(value: Int) extends Token
case class BOOLEAN_CONST(value: Boolean) extends Token

case class SEMICOLON() extends Token
case class LEFT_PARENTHESIS() extends Token
case class RIGHT_PARENTHESIS() extends Token
case class DOT() extends Token
case class ASSIGN() extends Token
case class COMMA() extends Token
case class COLON() extends Token
case class EQUALS() extends Token
case class PLUS() extends Token
case class MINUS() extends Token
case class TIMES() extends Token
case class DIVISION() extends Token
case class MODULO() extends Token
case class LESS() extends Token
case class LESS_EQUALS() extends Token
case class GREATER() extends Token
case class GREATER_EQUALS() extends Token
case class NOT_EQUALS() extends Token
case class AND() extends Token
case class OR() extends Token




