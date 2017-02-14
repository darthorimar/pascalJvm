import compiler.parser._
import compiler.typeChecker.TypeChecker
import org.scalatest._


class TypeCheckerTest extends FlatSpec with Matchers {

  "Type Checker" should "determine type of expression" in {
    val expression: Expression =
      BinaryOperatorExpression(Number(1),
        BinaryOperatorExpression(Number(2), Number(3), BinaryOperator.Times), BinaryOperator.Plus)

    TypeChecker.getExpressionType(expression)(null) shouldBe Right(Number)
  }

  "Type Checker" should "throw error" in {
    val expression: Expression =
      BinaryOperatorExpression(Number(2), BooleanConst(true), BinaryOperator.Times)

    TypeChecker.getExpressionType(expression)(null).isLeft shouldBe true
  }

  "Type Checker" should "type check ast" in {
    val ast: Program =
      Program(
        Header(List(
          VarDeclarationBlock(List(VarDeclarationList(List("a", "b"), compiler.parser.Number))),
          ConstDeclarationBlock(List(ConstDeclaration("c", 1))))),
        StatementBlock(List(
          IfStatement(
            BinaryOperatorExpression(VariableRef("c"), Number(1), BinaryOperator.Equals),
            StatementBlock(List(AssignStatement(VariableRef("a"), Number(1)))),
            Some(StatementBlock(List(AssignStatement(VariableRef("a"), Number(2)))))))))

    TypeChecker(ast).isEmpty shouldBe true
  }

  "Type Checker" should "type throw error on bad ast" in {
    val ast: Program =
      Program(
        Header(List(
          VarDeclarationBlock(List(VarDeclarationList(List("a", "b"), compiler.parser.Number))),
          ConstDeclarationBlock(List(ConstDeclaration("c", 1))))),
        StatementBlock(List(
          IfStatement(
            BinaryOperatorExpression(VariableRef("c"), Number(1), BinaryOperator.Equals),
            StatementBlock(List(AssignStatement(VariableRef("c"), Number(1)))),
            Some(StatementBlock(List(AssignStatement(VariableRef("a"), Number(2)))))))))

    TypeChecker(ast).isDefined shouldBe true
  }
}
