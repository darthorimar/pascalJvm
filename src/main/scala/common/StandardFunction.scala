package common
import compiler.codeGenerator.CodeGenerator
import compiler.codeGenerator.CodeGenerator.Scope
import compiler.parser.{ProcedureCall, ProcedureParameter}
import org.objectweb.asm.Opcodes._

trait StandardFunction {
  def generate(parameters: Seq[ProcedureParameter])(implicit scope: Scope)
}
case class WriteFunction() extends StandardFunction {
  override def generate(parameters: Seq[ProcedureParameter])(implicit scope: Scope): Unit = {
    scope.methodVisitor.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    val formatLine = "%d " * parameters.length
    scope.methodVisitor.visitLdcInsn(formatLine)

    scope.methodVisitor.visitIntInsn(SIPUSH, parameters.length)
    scope.methodVisitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
    for ((parameter, i) <- parameters.zipWithIndex) {
      //Let's assume that all params are ints for now
      scope.methodVisitor.visitInsn(DUP)

      scope.methodVisitor.visitIntInsn(SIPUSH, i)
      CodeGenerator.generateExpression(parameter.expression)
      scope.methodVisitor.visitMethodInsn(INVOKESTATIC,
        "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
      scope.methodVisitor.visitInsn(AASTORE)
    }

    scope.methodVisitor.visitMethodInsn(INVOKEVIRTUAL,
      "java/io/PrintStream", "printf", "(Ljava/lang/String;[Ljava/lang/Object;)Ljava/io/PrintStream;", false)
    scope.methodVisitor.visitInsn(POP)
  }
}


object StandardFunction {
  private val standardFunctionList =
    collection.immutable.HashMap[String, StandardFunction]("write" -> new WriteFunction)

  def generateStandardFunctionCall(procedureCall: ProcedureCall)(implicit scope: Scope): Unit =
    standardFunctionList.getOrElse(procedureCall.name, null).generate(procedureCall.parameters)

  def isStandardFunction(name: String): Boolean =
    standardFunctionList.contains(name)
}

