import java.io.{BufferedOutputStream, FileOutputStream}

import compiler.codeGenerator.CodeGenerator
import compiler.lexer.Lexer
import compiler.parser.Parser
import org.scalatest._


class CodeGeneratorTest extends FlatSpec with Matchers {

  private def tokenizeAndParse(code: String) =
    Parser(Lexer(code).right.get)

  "Code generator" should "work fine" in {
    val code = """
      |program;
      |  var a, b: Integer;
      |begin
      |  b := 42;
      |  a:= (1 + 2) / b;
      |end.
    """.stripMargin



    val bytes = CodeGenerator(tokenizeAndParse(code).right.get)

    val bos = new BufferedOutputStream(new FileOutputStream("out.class"))
    Stream.continually(bos.write(bytes))
    bos.close()
  }


}
