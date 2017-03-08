import common.BaseVariableType
import compiler.parser._
import compiler.typeChecker.TypeChecker
import org.scalatest._


class TypeCheckerTest extends FlatSpec with Matchers {

  "Type Checker" should "determine type of expression" in {
    val expression: Expression =
      BinaryOperatorExpression(IntegerLiteral(1),
        BinaryOperatorExpression(IntegerLiteral(2), IntegerLiteral(3), BinaryOperator.Times), BinaryOperator.Plus)

    TypeChecker.getExpressionType(expression)(null) shouldBe Right(IntegerLiteral)
  }

  "Type Checker" should "throw error" in {
    val expression: Expression =
      BinaryOperatorExpression(IntegerLiteral(2), BooleanLiteral(true), BinaryOperator.Times)

    TypeChecker.getExpressionType(expression)(null).isLeft shouldBe true
  }

  "Type Checker" should "type check ast" in {
    val ast: Program =
      Program(
        Header(List(
          VarDeclarationBlock(List(VarDeclarationList(List("a", "b"), VariableTypeNode(BaseVariableType.Number)))),
          ConstDeclarationBlock(List(ConstDeclaration("c", IntegerLiteral(1)))))),
        StatementBlock(List(
          IfStatement(
            BinaryOperatorExpression(VariableReference("c"), IntegerLiteral(1), BinaryOperator.Equals),
            StatementBlock(List(AssignStatement(VariableReference("a"), IntegerLiteral(1)))),
            Some(StatementBlock(List(AssignStatement(VariableReference("a"), IntegerLiteral(2)))))))))

    TypeChecker(ast).isEmpty shouldBe true
  }

  "Type Checker" should "type throw error on bad ast" in {
    val ast: Program =
      Program(
        Header(List(
          VarDeclarationBlock(List(VarDeclarationList(List("a", "b"), VariableTypeNode(BaseVariableType.Number)))),
          ConstDeclarationBlock(List(ConstDeclaration("c", IntegerLiteral(1)))))),
        StatementBlock(List(
          IfStatement(
            BinaryOperatorExpression(VariableReference("c"), IntegerLiteral(1), BinaryOperator.Equals),
            StatementBlock(List(AssignStatement(VariableReference("c"), IntegerLiteral(1)))),
            Some(StatementBlock(List(AssignStatement(VariableReference("a"), IntegerLiteral(2)))))))))

    TypeChecker(ast).isDefined shouldBe true
  }
}
