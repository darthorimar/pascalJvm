import java.io.{BufferedOutputStream, FileOutputStream}

import compiler.codeGenerator.CodeGenerator
import compiler.lexer.Lexer
import compiler.parser.Parser
import org.scalatest._


class CodeGeneratorTest extends FlatSpec with Matchers {

  private def tokenizeAndParse(code: String) = {
    val tokens = Lexer(code).right.get
    Parser(tokens)
  }

  "Code generator" should "work fine" in {
    val code =
      """
        |program;
        |  var a,b: Integer;
        |begin
        |  if true then
        |     if true then
        |   a :=3;
        |  else
        |    a:=3;
        |  else
        |  begin
        |    a := 4;
        |    a := 5;
        |  end;
        |end.""".stripMargin

    tokenizeAndParse(code) match {
      case Left(errors) => println(errors)
      case Right(bytes) =>
        val ast = tokenizeAndParse(code).right.get
        val bytes = CodeGenerator(ast)

        val bos = new BufferedOutputStream(new FileOutputStream("Main.class"))
        Stream.continually(bos.write(bytes))
        bos.close()
    }

  }


}
