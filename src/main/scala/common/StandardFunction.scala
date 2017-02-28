package common

import compiler.codeGenerator.CodeGenerator
import compiler.codeGenerator.CodeGenerator.Scope
import compiler.parser._
import org.objectweb.asm.Opcodes._

trait StandardFunction {
  def generate(parameters: Seq[ProcedureParameter])(implicit scope: Scope)
  val parameterTypes: Seq[_ >: VariableType]
}

case class ReadFunction() extends StandardFunction {
  override def generate(parameters: Seq[ProcedureParameter])(implicit scope: Scope): Unit = {
    parameters.foreach({ case ProcedureParameter(variableRef: VariableRef) =>
      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      scope.methodVisitor.visitFieldInsn(GETFIELD, scope.className, "scanner", "Ljava/util/Scanner;")
      scope.methodVisitor.visitMethodInsn(  INVOKEVIRTUAL, "java/util/Scanner", "nextInt", "()I", false)
      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      scope.methodVisitor.visitInsn(SWAP)
      scope.methodVisitor.visitFieldInsn(PUTFIELD, scope.className, variableRef.variable, "I")
    })
  }

  override val parameterTypes: Seq[_ >: VariableType] = Seq(+BaseVariableType.Number)
}

case class WriteFunction(newline: Boolean) extends StandardFunction {
  override def generate(parameters: Seq[ProcedureParameter])(implicit scope: Scope): Unit = {
    scope.methodVisitor.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    val formatLine = parameters.map { parameter =>
      parameter.expression.expressionType match {
        case BaseVariableType.String => "%s"
        case BaseVariableType.Number => "%d"
      }
    }.mkString(" ") + (if (newline) "\n" else "")
    scope.methodVisitor.visitLdcInsn(formatLine)

    scope.methodVisitor.visitIntInsn(SIPUSH, parameters.length)
    scope.methodVisitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
    for ((parameter, i) <- parameters.zipWithIndex) {
      scope.methodVisitor.visitInsn(DUP)

      scope.methodVisitor.visitIntInsn(SIPUSH, i)
      CodeGenerator.generateExpression(parameter.expression)
      parameter.expression.expressionType match {
        case BaseVariableType.Number =>
          scope.methodVisitor.visitMethodInsn(INVOKESTATIC,
            "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        case BaseVariableType.String =>
      }
      scope.methodVisitor.visitInsn(AASTORE)
    }

    scope.methodVisitor.visitMethodInsn(INVOKEVIRTUAL,
      "java/io/PrintStream", "printf", "(Ljava/lang/String;[Ljava/lang/Object;)Ljava/io/PrintStream;", false)
    scope.methodVisitor.visitInsn(POP)
  }

  override val parameterTypes: Seq[_ >: VariableType] = Seq(+(BaseVariableType.Number | BaseVariableType.String))
}

object StandardFunction {
    private val standardFunctionList =
    collection.immutable.HashMap[String, StandardFunction](
      "write" -> WriteFunction(false),
      "writeln" -> WriteFunction(true),
      "read" -> ReadFunction())


  def generateStandardFunctionCall(procedureCall: ProcedureCall)(implicit scope: Scope): Unit =
    standardFunctionList.getOrElse(procedureCall.name, null).generate(procedureCall.parameters)

  def isStandardFunction(name: String): Boolean =
    standardFunctionList.contains(name)

  def byName(name: String) : StandardFunction=
    standardFunctionList.getOrElse(name, null)
}

