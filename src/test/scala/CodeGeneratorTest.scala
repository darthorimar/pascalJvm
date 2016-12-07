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
        |  var a, b: boolean;
        |begin
        |  a := (1 * 3 - 4 < 2) and (3 >= 4);
        |end.
      """.stripMargin


    tokenizeAndParse(code) match {
      case Left(errors) => println(errors)
      case Right(bytes) =>
        val bytes = CodeGenerator(tokenizeAndParse(code).right.get)

        val bos = new BufferedOutputStream(new FileOutputStream("out.class"))
        Stream.continually(bos.write(bytes))
        bos.close()
    }

  }


}
