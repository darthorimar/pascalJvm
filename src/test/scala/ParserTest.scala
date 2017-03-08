import common.BaseVariableType
import compiler.lexer._
import compiler.parser._
import org.scalatest._


class ParserTest extends FlatSpec with Matchers {

  private def tokenizeAndParse(code: String) =
    Parser(Lexer(code).right.get)

  "Parser" should "parser empty program" in {
    val code =
      """
        |begin
        |end.
      """.stripMargin
    tokenizeAndParse(code).isRight should be(true)
  }

  "Parser" should "parse expression" in {
    val code =
      """
        |begin
        |  a := 1 + 2 * 3;
        |end.
      """.stripMargin
    val expectedAst = Program(
      Header(List()),
      StatementBlock(List(
        AssignStatement(VariableReference("a"),
          BinaryOperatorExpression(IntegerLiteral(1),
            BinaryOperatorExpression(IntegerLiteral(2), IntegerLiteral(3), BinaryOperator.Times), BinaryOperator.Plus)))))
    val a = tokenizeAndParse(code) shouldBe Right(expectedAst)
    println(a)
  }
  "Parser" should "parse simple program" in {
    val code =
      """
        |program;
        |  var a,b: Integer;
        |  const c = 1;
        |begin
        |  if c = 1 then a:=1; else a:=2;
        |end.
      """.stripMargin

    val expectedAst = Program(
      Header(List(
        VarDeclarationBlock(List(VarDeclarationList(List("a", "b"), VariableTypeNode(BaseVariableType.Number)))),
        ConstDeclarationBlock(List(ConstDeclaration("c", IntegerLiteral(1)))))),
      StatementBlock(List(
        IfStatement(
          BinaryOperatorExpression(VariableReference("c"), IntegerLiteral(1), BinaryOperator.Equals),
          StatementBlock(List(AssignStatement(VariableReference("a"), IntegerLiteral(1)))),
          Some(StatementBlock(List(AssignStatement(VariableReference("a"), IntegerLiteral(2)))))))))
    tokenizeAndParse(code) shouldBe Right(expectedAst)
  }
}
