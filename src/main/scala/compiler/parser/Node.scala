package compiler.parser

import common.{BaseVariableType, VariableType}
import compiler.parser.BinaryOperator.BinaryOperator
import compiler.parser.LoopType.LoopType

import scala.util.parsing.input.Positional


object BinaryOperator extends Enumeration {
  type BinaryOperator = Value
  val Plus = Value//("+")
  val Minus = Value//("-")
  val Times = Value//("*")
  val Division = Value//("/")
  val Modulo = Value//("%")
  val Less = Value//("<")
  val LessEqual = Value//("<=")
  val Greater = Value//(">")
  val GreaterEqual = Value//(">=")
  val Equals = Value//("=")
  val NotEquals = Value//("<>")
  val And = Value//("and")
  val Or = Value//("or")
}

case class VariableTypeNode(variableType : BaseVariableType) extends Positional

object LoopType extends Enumeration {
  type LoopType = Value
  val To = Value("to")
  val Downto = Value("downto")
}

sealed trait Node extends Positional
case class Program(header: Header, body: StatementBlock) extends Node

sealed trait Expression extends Node {
  var expressionType: VariableType = BaseVariableType.NoType
}

case class VariableRef(variable: String) extends Expression
case class Number(value: Integer) extends Expression
case class BooleanConst(value: Boolean) extends Expression
case class StringLiteral(value :String) extends Expression
case class BinaryOperatorExpression(exp1: Expression, exp2: Expression, op: BinaryOperator) extends Expression


sealed trait Statement extends Node
case class AssignStatement(variable: VariableRef, expr: Expression) extends Statement
case class IfStatement(condition: Expression, trueWay: StatementBlock,
                       falseWay: Option[StatementBlock]) extends Statement
case class WhileStatement(condition: Expression, statements: StatementBlock) extends Statement
case class ForStatement(counterVariable: VariableRef, from: Expression, loopType: LoopType,
                        to: Expression, statements: StatementBlock) extends Statement

case class ProcedureCall(name: String, parameters: Seq[ProcedureParameter]) extends Statement

case class ProcedureParameter(expression: Expression)


case class StatementBlock(statements: Seq[Statement]) extends Node

case class Header(declarations: Seq[Declaration]) extends Node


sealed trait Declaration extends Node

case class VarDeclarationBlock(declarations: Seq[VarDeclarationList]) extends Declaration
case class VarDeclarationList(vars: Seq[String], varType: VariableTypeNode) extends Node

case class ConstDeclarationBlock(declarations: Seq[ConstDeclaration]) extends Declaration
case class ConstDeclaration(name: String, expression: Expression) extends Node
