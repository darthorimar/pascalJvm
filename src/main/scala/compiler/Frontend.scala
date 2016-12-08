package compiler
import java.io.{BufferedOutputStream, FileOutputStream}

object Frontend extends App {
  if (args.length != 2) {
    println(
      """
        |Wrong number of arguments:
        |Arguments should be <sourceFile.pas> <outputClassName>
      """.stripMargin)
    System.exit(1)
  }
  val filePath = args(0)
  val className = args(1)

  Compiler.compileFile(filePath, className) match {
    case Left(errors) => println("Compilation errors:"); errors.foreach(println)
    case Right(bytes) =>
      val outputStream = new BufferedOutputStream(new FileOutputStream(s"$className.class"))
      Stream.continually(outputStream.write(bytes))
      outputStream.close()
  }
}
