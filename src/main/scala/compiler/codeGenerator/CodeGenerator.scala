package compiler.codeGenerator

import compiler.parser._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor}

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
        val fieldVisitor = scope.classWriter.visitField(ACC_PRIVATE, variable, variablesType.toJvmType, null, null)
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

      statements.foreach(generateCode(_)(CodeGeneratorScope(scope.classWriter, methodVisitor)))

      methodVisitor.visitInsn(RETURN)
      methodVisitor.visitMaxs(1, 1) //params will be ignored because of COMPUTE_FRAMES used
      methodVisitor.visitEnd()

    case AssignStatement(variable, expr) =>
      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      generateCode(expr)
      scope.methodVisitor.visitFieldInsn(PUTFIELD, "pascalJvm/Main", variable.variable, "I")

    case Number(value) =>
      scope.methodVisitor.visitIntInsn(BIPUSH, value)

    case BooleanConst(value) =>
      scope.methodVisitor.visitIntInsn(BIPUSH, if (value) 1 else 0)

    case BinaryOperatorExpression(exp1, exp2, op) =>
      generateCode(exp1)
      generateCode(exp2)

      def comparationExpression(operatorCode : Int) = {
        val falseWay = new Label
        val continue = new Label
        scope.methodVisitor.visitJumpInsn(operatorCode, falseWay)
        scope.methodVisitor.visitInsn(ICONST_1)
        scope.methodVisitor.visitJumpInsn(GOTO, continue)

        scope.methodVisitor.visitLabel(falseWay)
        scope.methodVisitor.visitFrame(F_SAME, 0, null, 0, null)
        scope.methodVisitor.visitInsn(ICONST_0)
        scope.methodVisitor.visitLabel(continue)
        scope.methodVisitor.visitFrame(F_SAME, 0, null, 0, null)
      }

      op match {
        case BinaryOperator.Plus => scope.methodVisitor.visitInsn(IADD)
        case BinaryOperator.Minus => scope.methodVisitor.visitInsn(ISUB)
        case BinaryOperator.Times => scope.methodVisitor.visitInsn(IMUL)
        case BinaryOperator.Division => scope.methodVisitor.visitInsn(IDIV)
        case BinaryOperator.Modulo => scope.methodVisitor.visitInsn(IREM)

        //No short-circuiting for now)
        case BinaryOperator.And => scope.methodVisitor.visitInsn(IAND)
        case BinaryOperator.Or => scope.methodVisitor.visitInsn(IOR)

        case BinaryOperator.Less => comparationExpression(IF_ICMPGE)
        case BinaryOperator.LessEqual => comparationExpression(IF_ICMPGT)
        case BinaryOperator.Greater => comparationExpression(IF_ICMPLE)
        case BinaryOperator.GreaterEqual => comparationExpression(IF_ICMPLT)


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
