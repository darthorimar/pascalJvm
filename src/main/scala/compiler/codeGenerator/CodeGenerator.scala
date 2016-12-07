package compiler.codeGenerator

import compiler.parser._
import org.objectweb.asm.{MethodVisitor, ClassWriter}
import org.objectweb.asm.Opcodes._

import scala.collection.mutable


object CodeGenerator {

  val constToInit: mutable.Map[String, Int] = new mutable.HashMap[String, Int]

  case class CodeGeneratorScope(classWriter: ClassWriter, methodVisitor: MethodVisitor)


  private def generateCode(node: Node)(implicit scope: CodeGeneratorScope): Unit = node match {

    case Program(header, body) =>
      scope.classWriter.visit(52, ACC_PUBLIC + ACC_SUPER, "pascalJvm/Main", null, "java/lang/Object", null)
      generateCode(header)
      generateCode(body)
      scope.classWriter.visitEnd()

    case Header(declarations) =>
      declarations.foreach(generateCode)

    case VarDeclarationBlock(declarations) =>
      declarations.foreach(generateCode)

    case VarDeclarationList(variables, variablesType) =>
      variables.foreach(variable => {
        val fieldVisitor = scope.classWriter.visitField(ACC_PRIVATE, variable, "I", null, null)
        fieldVisitor.visitEnd()
      })
    case ConstDeclarationBlock(declarations) =>
      declarations.foreach(generateCode)

    case ConstDeclaration(variable, value) =>
      println(s"$variable $value")
      val fieldVisitor = scope.classWriter.visitField(ACC_PRIVATE + ACC_FINAL, variable, "I", null, null)
      fieldVisitor.visitEnd()
      constToInit.put(variable, value);

    case StatementBlock(statements) =>
      val methodVisitor = scope.classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)

      methodVisitor.visitVarInsn(ALOAD, 0)
      methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)

      constToInit.foreach { case (variable, value) =>
        methodVisitor.visitVarInsn(ALOAD, 0)
        methodVisitor.visitIntInsn(BIPUSH, value)
        methodVisitor.visitFieldInsn(PUTFIELD, "pascalJvm/Main", variable, "I");
      }

      statements.foreach(generateCode(_)( CodeGeneratorScope(scope.classWriter, methodVisitor)))

      methodVisitor.visitInsn(RETURN)
      methodVisitor.visitMaxs(1, 1) //params will be ignored because of COMPUTE_FRAMES used
      methodVisitor.visitEnd()

    case AssignStatement(variable, expr) =>
      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      generateCode(expr)
      scope.methodVisitor.visitFieldInsn(PUTFIELD, "pascalJvm/Main",variable.variable, "I")

    case Number(value) =>
      scope.methodVisitor.visitIntInsn(BIPUSH, value)

    case BinaryOperatorExpression(exp1, exp2, op) =>
      generateCode(exp1)
      generateCode(exp2)
      op match {
        case BinaryOperator.Plus => scope.methodVisitor.visitInsn(IADD)
        case BinaryOperator.Minus => scope.methodVisitor.visitInsn(ISUB)
        case BinaryOperator.Times => scope.methodVisitor.visitInsn(IMUL)
        case BinaryOperator.Division => scope.methodVisitor.visitInsn(IDIV)
        case BinaryOperator.Modulo => scope.methodVisitor.visitInsn(IREM)
      }

    case VariableRef(variable) =>
      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      scope.methodVisitor.visitFieldInsn(GETFIELD, "pascalJvm/Main", variable, "I")

      scope.classWriter.toByteArray
  }

  def apply(ast: Program) = {
    val classWriter: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)

    generateCode(ast)(CodeGeneratorScope(classWriter, null))
    classWriter.toByteArray
  }
}
