package compiler
import compiler.codeGenerator.CodeGenerator
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.typeChecker.TypeChecker

object Compiler {
  def compile(code: String, className: String): Either[List[CompilationError], Array[Byte]] = {
    Lexer(code) match {
      case Left(errors) => Left(errors)
      case Right(tokens) =>
        Parser(tokens) match {
          case Left(errors) => Left(errors)
          case Right(ast) =>
            TypeChecker(ast) match {
              case Some(errors) => Left(errors)
              case None =>
                Right(CodeGenerator(ast, className))
            }
        }
    }
  }

  def compileFile(filePath: String, className: String): Either[Seq[CompilationError], Array[Byte]] = {
    val file = scala.io.Source.fromFile(filePath)
    val source = try file.mkString finally file.close()
    compile(source, className)
  }
}
