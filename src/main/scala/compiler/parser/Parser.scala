package compiler.parser

import compiler.lexer._
import compiler.parser.BinaryOperator.BinaryOperator
import compiler.parser.LoopType.LoopType
import compiler.{Location, ParserError}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Parser extends Parsers {
  override type Elem = Token

  private def identifier =
    accept("identifier", { case IDENTIFIER(name) => name.toLowerCase })

  private def number =
    accept("number", { case NUMBER(value) => Number(value) })

  private def booleanConst =
    accept("bool const", { case BOOLEAN_CONST(value) => BooleanConst(value) })

  def nonEmptyListWithSeparator[I, S](item: Parser[I], separator: Parser[S]) =
    item ~ rep(separator ~ item) ^^ {
      case first ~ others => first :: others.map(_._2)
    }

  implicit def toOperator(token: Token): BinaryOperator = token match {
    case PLUS() => BinaryOperator.Plus
    case MINUS() => BinaryOperator.Minus
    case TIMES() => BinaryOperator.Times
    case DIVISION() => BinaryOperator.Division
    case MODULO() => BinaryOperator.Modulo
    case AND() => BinaryOperator.And
    case OR() => BinaryOperator.Or
    case LESS() => BinaryOperator.Less
    case LESS_EQUALS() => BinaryOperator.LessEqual
    case GREATER() => BinaryOperator.Greater
    case GREATER_EQUALS() => BinaryOperator.GreaterEqual
    case EQUALS() => BinaryOperator.Equals
    case NOT_EQUALS() => BinaryOperator.NotEquals
  }

  implicit def toLoopType(loopType: String): LoopType = LoopType.withName(loopType)

  def binaryOperator(expr: Parser[Expression], op: Parser[Token]) =
    expr ~ rep(op ~ expr) ^^ {
      case expr1 ~ others => others.foldLeft(expr1)((e, es) => BinaryOperatorExpression(e, es._2, es._1))
    }

  def variableRef = identifier ^^ VariableRef

  def expression = positioned {
    def parExpresssion: Parser[Expression] = LEFT_PARENTHESIS() ~> lastExpression <~ RIGHT_PARENTHESIS()
    def firstExpression = number | booleanConst | variableRef | parExpresssion
    def secondExpression = binaryOperator(firstExpression, TIMES() | DIVISION() | MODULO() | AND())
    def thirdExpression = binaryOperator(secondExpression, PLUS() | MINUS() | OR())
    def lastExpression = binaryOperator(thirdExpression,
      EQUALS() | NOT_EQUALS() | LESS() | LESS_EQUALS() | GREATER() | GREATER_EQUALS())

    lastExpression
  }

  def block: Parser[StatementBlock] = positioned {
    val withBeginEnd = IDENTIFIER("begin") ~> rep(statement) <~ IDENTIFIER("end") ~ SEMICOLON() ^^
      (statements => StatementBlock(statements))
    val withoutBeginEnd = statement ^^ (statement => StatementBlock(List(statement)))
    withBeginEnd | withoutBeginEnd
  }

  def statement: Parser[Statement] = positioned {
    {
      val assignStatement = variableRef ~ ASSIGN() ~ expression ~ SEMICOLON() ^^ {
        case variable ~ _ ~ expr ~ _ => AssignStatement(variable, expr)
      }
      val ifStatement = IDENTIFIER("if") ~ expression ~ IDENTIFIER("then") ~ block ~ opt(IDENTIFIER("else") ~> block) ^^ {
        case _ ~ expr ~ _ ~ trueWay ~ falseWay => IfStatement(expr, trueWay, falseWay)
      }
      val whileStatement = IDENTIFIER("while") ~ expression ~ IDENTIFIER("do") ~ block ^^ {
        case _ ~ expr ~ _ ~ loopBody => WhileStatement(expr, loopBody)
      }
      val forStatement = IDENTIFIER("for") ~ variableRef ~ ASSIGN() ~ expression ~
        (IDENTIFIER("to") | IDENTIFIER("downto")) ~ expression ~ IDENTIFIER("do") ~ block ^^ {
        case _ ~ variableName ~ _ ~ from ~ IDENTIFIER(loopType) ~ to ~ _ ~ loopBody
        => ForStatement(variableName, from, loopType, to, loopBody)
      }

      assignStatement | ifStatement | whileStatement | forStatement
    }
  }

  def body = positioned {
    IDENTIFIER("begin") ~> rep(statement) <~ IDENTIFIER("end") ^^ (statements => StatementBlock(statements))
  }

  def variableType = positioned {identifier ^^ (varType => VariableType(varType))}

  def declaration = positioned {
    def varDeclarationBlock = IDENTIFIER("var") ~ rep1(varDeclarationList) ^^ {
      case _ ~ declarations => VarDeclarationBlock(declarations)
    }

    def varDeclarationList = nonEmptyListWithSeparator(identifier, COMMA()) ~ COLON() ~ variableType ~ SEMICOLON() ^^ {
      case vars ~ _ ~ varType ~ _ => VarDeclarationList(vars, varType)
    }

    def constDeclarationBlock = IDENTIFIER("const") ~ rep1(constDeclaration) ^^ {
      case _ ~ declarations => ConstDeclarationBlock(declarations)
    }

    def constDeclaration = identifier ~ EQUALS() ~ number ~ SEMICOLON() ^^ {
      case name ~ _ ~ Number(value) ~ _ => ConstDeclaration(name, value)
    }

    varDeclarationBlock | constDeclarationBlock
  }

  def header = positioned {
    rep(declaration) ^^ Header
  }

  def program = positioned {
    opt(IDENTIFIER("program") ~ SEMICOLON()) ~> header ~ body <~ phrase(DOT()) ^^ {
      case programHeader ~ programBody => Program(programHeader, programBody)
    }
  }

  def apply(tokens: Seq[Token]) = {
    val reader = new TokenReader(tokens)
    program(reader) match {
      case Success(result, _) => Right(result)
      case NoSuccess(message, next) =>
        Left(List(ParserError(Location(next.first.pos), message)))
    }
  }
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head

  override def rest: Reader[Token] = new TokenReader(tokens.tail)

  override def pos: Position = NoPosition

  override def atEnd: Boolean = tokens.isEmpty
}