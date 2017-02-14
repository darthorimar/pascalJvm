package compiler.codeGenerator

import common.StandardFunction
import compiler.parser._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor}

import scala.collection.mutable


object CodeGenerator {

  val constToInit: mutable.Map[String, Int] = new mutable.HashMap[String, Int]

  case class Scope(classWriter: ClassWriter, methodVisitor: MethodVisitor, className: String)

  def generateExpression(expression: Expression)(implicit scope: Scope) = expression match {
    case Number(value) =>
     scope.methodVisitor.visitLdcInsn(new Integer(value))

    case BooleanConst(value) =>
      scope.methodVisitor.visitIntInsn(BIPUSH, if (value) 1 else 0)

    case BinaryOperatorExpression(exp1, exp2, op) =>
      generateCode(exp1)
      generateCode(exp2)

      def compareExpression(operatorCode: Int) = {
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
      scope.methodVisitor.visitFieldInsn(GETFIELD, scope.className, variable, "I")
  }

  private def generateCode(node: Node)(implicit scope: Scope): Unit = node match {

    case Program(header, body) =>
      scope.classWriter.visit(52, ACC_PUBLIC + ACC_SUPER, scope.className, null, "java/lang/Object", null)
      generateScannerVariable()
      generateCode(header)
      generateBody()
      generateMainMethod()
      scope.classWriter.visitEnd()

      def generateScannerVariable() = {
        val fieldVisitor = scope.classWriter.visitField(ACC_PRIVATE, "scanner", "Ljava/util/Scanner;", null, null)
        fieldVisitor.visitEnd()
      }

      def generateBody() = {
        val methodVisitor = scope.classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)

        methodVisitor.visitVarInsn(ALOAD, 0)
        methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)

        constToInit.foreach { case (variable, value) =>
          methodVisitor.visitVarInsn(ALOAD, 0)
          methodVisitor.visitLdcInsn(new Integer(value))
          methodVisitor.visitFieldInsn(PUTFIELD, scope.className, variable, "I");
        }

        methodVisitor.visitVarInsn(ALOAD, 0)
        methodVisitor.visitTypeInsn(NEW, "java/util/Scanner")
        methodVisitor.visitInsn(DUP)
        methodVisitor.visitFieldInsn(GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;")
        methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/util/Scanner", "<init>", "(Ljava/io/InputStream;)V", false)
        methodVisitor.visitFieldInsn(PUTFIELD, scope.className, "scanner", "Ljava/util/Scanner;")

        generateCode(body)(Scope(scope.classWriter, methodVisitor, scope.className))
        methodVisitor.visitInsn(RETURN)
        methodVisitor.visitMaxs(1, 1) //params will be ignored because of COMPUTE_FRAMES used
        methodVisitor.visitEnd()
      }

      def generateMainMethod() = {
        val methodVisitor = scope.classWriter.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
        methodVisitor.visitCode()
        methodVisitor.visitTypeInsn(NEW, scope.className)
        methodVisitor.visitMethodInsn(INVOKESPECIAL, scope.className, "<init>", "()V", false)
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
      scope.methodVisitor.visitFieldInsn(PUTFIELD, scope.className, variable.variable, "I")

    case expression: Expression =>
      generateExpression(expression)

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

    case WhileStatement(condition, statements) =>
      val beginLabel = new Label
      val continueLabel = new Label

      scope.methodVisitor.visitLabel(beginLabel)
      scope.methodVisitor.visitFrame(F_SAME, 0, null, 0, null)
      generateCode(condition)
      scope.methodVisitor.visitInsn(ICONST_1)
      scope.methodVisitor.visitJumpInsn(IF_ICMPNE, continueLabel)
      generateCode(statements)
      scope.methodVisitor.visitJumpInsn(GOTO, beginLabel)
      scope.methodVisitor.visitLabel(continueLabel)
      scope.methodVisitor.visitFrame(F_SAME, 0, null, 0, null)

    case ForStatement(counterVariable, from, loopType, to, statements) =>
      val beginLabel = new Label
      val continueLabel = new Label

      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      generateCode(from)
      scope.methodVisitor.visitFieldInsn(PUTFIELD, scope.className, counterVariable.variable, "I")

      generateCode(to)

      scope.methodVisitor.visitLabel(beginLabel)
      scope.methodVisitor.visitFrame(F_SAME, 0, null, 0, null)

      scope.methodVisitor.visitInsn(DUP)
      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      scope.methodVisitor.visitFieldInsn(GETFIELD, scope.className, counterVariable.variable, "I")
      scope.methodVisitor.visitInsn(SWAP)

      loopType match {
        case LoopType.To => scope.methodVisitor.visitJumpInsn(IF_ICMPGT, continueLabel)
        case LoopType.Downto => scope.methodVisitor.visitJumpInsn(IF_ICMPLT, continueLabel)
      }

      generateCode(statements)

      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      scope.methodVisitor.visitVarInsn(ALOAD, 0)
      scope.methodVisitor.visitFieldInsn(GETFIELD, scope.className, counterVariable.variable, "I")
      scope.methodVisitor.visitInsn(ICONST_1)
      loopType match {
        case LoopType.To => scope.methodVisitor.visitInsn(IADD)
        case LoopType.Downto => scope.methodVisitor.visitInsn(ISUB)
      }
      scope.methodVisitor.visitFieldInsn(PUTFIELD, scope.className, counterVariable.variable, "I")

      scope.methodVisitor.visitJumpInsn(GOTO, beginLabel)

      scope.methodVisitor.visitLabel(continueLabel)
      scope.methodVisitor.visitFrame(F_SAME, 0, null, 0, null)
      scope.methodVisitor.visitInsn(POP)

    case procedureCall@ProcedureCall(name, parameters) =>
      if (StandardFunction.isStandardFunction(name)) {
          StandardFunction.generateStandardFunctionCall(procedureCall)
      }
  }


  def apply(ast: Program, className: String) = {
    val classWriter: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)

    generateCode(ast)(Scope(classWriter, null, className.capitalize))
    classWriter.toByteArray
  }
}
