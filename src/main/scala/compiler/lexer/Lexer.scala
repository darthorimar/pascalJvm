package compiler.lexer

import compiler.{LexerError, Location}

import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {

  def identifier = positioned {
    """[A-Za-z\_][A-Za-z0-9\_]*""".r ^^ IDENTIFIER.compose(_.toLowerCase)
  }

  def number = positioned {
    "-?[0-9]+".r ^^ NUMBER.compose(_.toInt)
  }

  def booleanConst = positioned {
    """(?i)(true|false)""".r ^^ BOOLEAN_CONST.compose(_.toBoolean)
  }

  def string = positioned {
    """'[^']*'""".r ^^ STRING_LITERAL.compose(_.drop(1).dropRight(1))
  }

  def semicolon = positioned {";" ^^ (_ => SEMICOLON())}
  def colon = positioned {":" ^^ (_ => COLON())}
  def leftParenthesis = positioned {"(" ^^ (_ => LEFT_PARENTHESIS())}
  def rightParenthesis = positioned {")" ^^ (_ => RIGHT_PARENTHESIS())}
  def dot = positioned {"." ^^ (_ => DOT())}
  def assign = positioned {":=" ^^ (_ => ASSIGN())}
  def comma = positioned {"," ^^ (_ => COMMA())}
  def plus = positioned {"+" ^^ (_ => PLUS())}
  def minus = positioned {"-" ^^ (_ => MINUS())}
  def times = positioned {"*" ^^ (_ => TIMES())}
  def div = positioned {"/" ^^ (_ => DIVISION())}
  def modulo = positioned {"%" ^^ (_ => MODULO())}
  def less = positioned {"<" ^^ (_ => LESS())}
  def lessEquals = positioned {"<=" ^^ (_ => LESS_EQUALS())}
  def greater = positioned {">" ^^ (_ => GREATER())}
  def greaterEquals = positioned {">=" ^^ (_ => GREATER_EQUALS())}
  def notEquals = positioned {"<>" ^^ (_ => NOT_EQUALS())}
  def equals = positioned {"=" ^^ (_ => EQUALS())}


  def and = positioned {"(?i)and".r ^^ (_ => AND())}
  def or = positioned {"(?i)or".r ^^ (_ => OR())}


  def tokens: Parser[List[Token]] =
    phrase(rep1(number | notEquals | lessEquals | less | greaterEquals | greater | equals
      | plus | minus | times | div | modulo
      | assign | colon | comma | dot | leftParenthesis | rightParenthesis | semicolon
      | and | or | booleanConst | identifier | string))

  def apply(code: String): Either[List[LexerError], List[Token]] =
    parse(tokens, code) match {
      case Success(result, _) => Right(result)
      case NoSuccess(message, next) => Left(List(LexerError(Location(next.pos), message)))
    }
}

