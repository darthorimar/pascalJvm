package compiler.typeChecker

import compiler.parser.BinaryOperator.BinaryOperator
import compiler.parser.{BinaryOperator, _}
import compiler.{Location, TypeCheckError}


object TypeChecker {

  private type CheckResult = Option[List[TypeCheckError]]

  private def collectErrors(first: CheckResult, second: CheckResult): CheckResult = {
    Option(first.getOrElse(List.empty) ++ second.getOrElse(List.empty)).filterNot(_.isEmpty)
  }

  private def collectErrors(results: Seq[CheckResult]): CheckResult = {
    if (results.nonEmpty) results reduceLeft collectErrors else None
  }

  class Identifier(val name: String, val itemType: VariableType, val isVariable: Boolean)

  case class Scope(variables: collection.mutable.Map[String, Identifier])

  private def makeError(message: String, node: Node) =
    List(TypeCheckError(Location(node.pos), message))

  def checkExpressionType(expression: Expression,
                                  variableType: VariableType, expressionName: String)
                                 (implicit scope: Scope): CheckResult = {
    getExpressionType(expression) match {
      case Left(errors) => Some(errors)
      case Right(varType) =>
        Option(makeError(s"$expressionName must have type $variableType", expression)).filter(_ => varType != variableType)
    }
  }


  private def getOperatorDescription(operator: BinaryOperator): (Seq[VariableType], VariableType) = operator match {
    case BinaryOperator.Plus
         | BinaryOperator.Minus
         | BinaryOperator.Times
         | BinaryOperator.Division
         | BinaryOperator.Modulo
    => (List(Number), Number)

    case BinaryOperator.Less
         | BinaryOperator.LessEqual
         | BinaryOperator.Greater
         | BinaryOperator.GreaterEqual
    => (List(Number), Boolean)

    case BinaryOperator.Equals
         | BinaryOperator.NotEquals
    => (List(Number, Boolean), Boolean)

    case BinaryOperator.And
         | BinaryOperator.Or
    => (List(Boolean), Boolean)
  }

  def getExpressionType(expression: Expression)(implicit scope: Scope): Either[List[TypeCheckError], VariableType] = {
    expression match {
      case VariableRef(variable) =>
        Either.cond(scope.variables.contains(variable), scope.variables(variable).itemType,
          makeError(s"Identifier $variable not found", expression))

      case Number(_) => Right(Number)

      case BooleanConst(_) => Right(Boolean)

      case BinaryOperatorExpression(expression1, expression2, operator) =>
        val expression1Type = getExpressionType(expression1)
        val expression2Type = getExpressionType(expression2)
        if (expression1Type.isLeft || expression2Type.isLeft)
          Left(expression1Type.fold(identity, _ => List.empty) ++ expression2Type.fold(identity, _ => List.empty))
        else if (expression1Type != expression2Type)
          Left(makeError(s"Types of operands of operator `$operator` must be same", expression1))
        else {
          val (operandTypes: List[VariableType], resultType) = getOperatorDescription(operator)
          Either.cond(operandTypes.contains(expression1Type.right.get),
            resultType,
            makeError(s"Can not apply operator `$operator` to an $expression1Type expression", expression1))
        }
    }
  }

  private def typeCheck(node: Node)(implicit scope: Scope): Option[List[TypeCheckError]] = node match {
    case Program(header, body) => collectErrors(typeCheck(header), typeCheck(body))

    case Header(declarations) => collectErrors(declarations.map(typeCheck))

    case VarDeclarationBlock(variables) => collectErrors(variables.map(typeCheck))

    case ConstDeclarationBlock(variables) => collectErrors(variables.map(typeCheck))

    case expression: Expression =>
      val expressionType = getExpressionType(expression)
      if (expressionType.isRight)
        expression.expressionType = expressionType.right.get
      expressionType.fold(Some(_), _ => None)

    case node@VarDeclarationList(variables, variableType) =>
      val result = variables.map(name =>
        if (!scope.variables.contains(name)) {
          scope.variables.put(name, new Identifier(name, Number, true))
          None
        }
        else
          Some(makeError(s"Identifier `$name` is already defined", node))
      )
      collectErrors(result)

    case node@ConstDeclaration(name, value) =>
      if (!scope.variables.contains(name)) {
        scope.variables.put(name, new Identifier(name, Number, false))
        None
      }
      else
        Some(makeError(s"Identifier `$name` is already defined", node))

    case StatementBlock(statements) => collectErrors(statements.map(typeCheck))

    case node@AssignStatement(variable, expression) =>
      val variableType = scope.variables.get(variable.variable) match {
        case Some(identifier) =>
          Option(makeError("Can not assign to const", node)).filter(_ => !identifier.isVariable)
        case None => Some(makeError(s"Variable `${variable.variable}` not found", variable))
      }
      collectErrors(variableType, typeCheck(expression))

    case IfStatement(condition, trueWay, falseWay) =>
      val conditionType = checkExpressionType(condition, Boolean, "Condition expression")
      val trueWayType = typeCheck(trueWay)
      val falseWayType = falseWay.flatMap(typeCheck)
      collectErrors(List(conditionType, trueWayType, falseWayType))

    case WhileStatement(condition, statements) =>
      val conditionType = checkExpressionType(condition, Boolean, "Condition expression")
      val statementsType = typeCheck(statements)
      collectErrors(conditionType, statementsType)

    case ForStatement(counterVariable, from, loopType, to, statements) =>
      val counterVariableError = checkExpressionType(counterVariable, Number, "Counter variable")
      val fromError = checkExpressionType(from, Number, "Range value")
      val toError = checkExpressionType(to, Number, "Range value")
      val statementsErrors = typeCheck(statements)
      collectErrors(List(counterVariableError, fromError, toError, statementsErrors))
    case ProcedureCall(_, _) => collectErrors(List())
  }

  def apply(ast: Program): Option[List[TypeCheckError]] =
    typeCheck(ast)(Scope(collection.mutable.Map[String, Identifier]()))
}
