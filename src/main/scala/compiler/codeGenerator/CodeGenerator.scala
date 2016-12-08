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
      scope.classWriter.visit(52, ACC_PUBLIC + ACC_SUPER, "Main", null, "java/lang/Object", null)
      generateCode(header)
      generateBody()
      generateMainMethod()
      scope.classWriter.visitEnd()

      def generateBody() = {
        val methodVisitor = scope.classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)

        methodVisitor.visitVarInsn(ALOAD, 0)
        methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)

        constToInit.foreach { case (variable, value) =>
          methodVisitor.visitVarInsn(ALOAD, 0)
          methodVisitor.visitIntInsn(BIPUSH, value)
          methodVisitor.visitFieldInsn(PUTFIELD, "Main", variable, "I");
        }

        generateCode(body)(CodeGeneratorScope(scope.classWriter, methodVisitor))
        methodVisitor.visitInsn(RETURN)
        methodVisitor.visitMaxs(1, 1) //params will be ignored because of COMPUTE_FRAMES used
        methodVisitor.visitEnd()
      }

      def generateMainMethod() =  {
        val methodVisitor = scope.classWriter.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
        methodVisitor.visitCode()

        methodVisitor.visitTypeInsn(NEW, "Main")
        methodVisitor.visitInsn(DUP)
        methodVisitor.visitMethodInsn(INVOKESPECIAL, "Main", "<init>", "()V", false)
        methodVisitor.visitInsn(POP)
        methodVisitor.visitInsn(RETURN)

        methodVisitor.visitMaxs(2, 1)
        methodVisitor.visitEnd()
      }


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
      statements.foreach(generateCode)

    case AssignStatement(variable, expr) =>
      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      generateCode(expr)
      scope.methodVisitor.visitFieldInsn(PUTFIELD, "Main", variable.variable, "I")

    case Number(value) =>
      scope.methodVisitor.visitIntInsn(BIPUSH, value)

    case BooleanConst(value) =>
      scope.methodVisitor.visitIntInsn(BIPUSH, if (value) 1 else 0)

    case BinaryOperatorExpression(exp1, exp2, op) =>
      generateCode(exp1)
      generateCode(exp2)

      def compareExpression(operatorCode : Int) = {
        val falseWayLabel = new Label
        val continueLabel = new Label
        scope.methodVisitor.visitJumpInsn(operatorCode, falseWayLabel)
        scope.methodVisitor.visitInsn(ICONST_1)
        scope.methodVisitor.visitJumpInsn(GOTO, continueLabel)

        scope.methodVisitor.visitLabel(falseWayLabel)
        scope.methodVisitor.visitFrame(F_SAME, 0, null, 0, null)
        scope.methodVisitor.visitInsn(ICONST_0)
        scope.methodVisitor.visitLabel(continueLabel)
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

        case BinaryOperator.Less => compareExpression(IF_ICMPGE)
        case BinaryOperator.LessEqual => compareExpression(IF_ICMPGT)
        case BinaryOperator.Greater => compareExpression(IF_ICMPLE)
        case BinaryOperator.GreaterEqual => compareExpression(IF_ICMPLT)
        case BinaryOperator.Equals => compareExpression(IF_ICMPNE)
        case BinaryOperator.NotEquals => compareExpression(IF_ICMPEQ);
      }

    case VariableRef(variable) =>
      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      scope.methodVisitor.visitFieldInsn(GETFIELD, "Main", variable, "I")

      scope.classWriter.toByteArray

    case IfStatement(condition, trueWay, falseWay) =>
      generateCode(condition)
      val falseWayLabel = new Label
      val continueLabel = new Label
      scope.methodVisitor.visitInsn(ICONST_1)
      scope.methodVisitor.visitJumpInsn(IF_ICMPNE, falseWayLabel)
      generateCode(trueWay)
      scope.methodVisitor.visitJumpInsn(GOTO, continueLabel)
      scope.methodVisitor.visitLabel(falseWayLabel)
      scope.methodVisitor.visitFrame(F_SAME, 0, null, 0, null)
      if (falseWay.isDefined) generateCode(falseWay.get)
      scope.methodVisitor.visitLabel(continueLabel)
      scope.methodVisitor.visitFrame(F_SAME, 0, null, 0, null)
  }

  def apply(ast: Program) = {
    val classWriter: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)

    generateCode(ast)(CodeGeneratorScope(classWriter, null))
    classWriter.toByteArray
  }
}
