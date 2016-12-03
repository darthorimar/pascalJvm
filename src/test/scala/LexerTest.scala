import compiler.lexer._
import org.scalatest._


class LexerTest extends FlatSpec with Matchers {

  "Lexer" should "parse identifiers, numbers and boolean consts" in {
    val code = "ident 42 true"
    Lexer(code) shouldBe Right(List(IDENTIFIER("ident"), NUMBER(42), BOOLEAN_CONST(true)))
  }

  "Lexer" should "parse operators" in {
    val code = "<= and"
    Lexer(code) shouldBe Right(List(LESS_EQUALS(), AND()))
  }

  "Lexer" should "ignore case" in {
    val code = "BeGiN"
    Lexer(code) shouldBe Right(List(IDENTIFIER("begin")))
  }

  "Lexer" should "should throw error on invalid code" in {
    val code = "^"
    Lexer(code).isLeft should be(true)
  }
}
