package decaf.backend.jvm

import decaf.driver.{Config, Phase}
import decaf.frontend.annot.SymbolizedImplicit._
import decaf.frontend.annot.TypedImplicit._
import decaf.frontend.annot.{ArrayType, BaseType, BoolType, ClassType, FunType, IntType, JNative, LocalVarSymbol,
  NoType, StringType}
import decaf.frontend.tree.TreeNode
import decaf.frontend.tree.TreeNode.{ArithOp, EqOrCmpOp}
import decaf.frontend.tree.TypedTree._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes}

import scala.collection.mutable

class JVMGen extends Phase[Tree, List[JVMClass]]("jvm") with Util {

  override def transform(input: Tree): List[JVMClass] = input.classes.map(emitClass)

  /**
    * Generate bytecode for a decaf class.
    *
    * @param clazz the class
    * @return the wrapped class file
    */
  def emitClass(clazz: ClassDef): JVMClass = {
    implicit val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES + ClassWriter.COMPUTE_MAXS)

    // As said in https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.1-200-E.1:
    //   In Java SE 8 and above, the Java Virtual Machine considers the ACC_SUPER flag to be set in every class file,
    //   regardless of the actual value of the flag in the class file and the version of the class file.
    // Thus, we always set on ACC_SUPER flag and let a non-inherited decaf class extend java.lang.Object.
    val access = Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER
    val superClass = clazz.parent.map(internalName).getOrElse(JAVA_SUPER_INTERNAL_NAME)
    cw.visit(52, access, clazz.name, null, superClass, null)

    // First add the default constructor:
    val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, CONSTRUCTOR_NAME, CONSTRUCTOR_DESC, null, null)
    mv.visitCode()
    mv.visitVarInsn(Opcodes.ALOAD, 0)
    mv.visitMethodInsn(Opcodes.INVOKESPECIAL, superClass, CONSTRUCTOR_NAME, CONSTRUCTOR_DESC, false) // call super
    mv.visitInsn(Opcodes.RETURN)
    mv.visitMaxs(-1, -1)
    mv.visitEnd()

    // Then generate every user-defined member:
    clazz.fields.foreach {
      case field: VarDef =>
        cw.visitField(Opcodes.ACC_PROTECTED, field.name, descriptor(field.symbol), null, null)
      case method: MethodDef => emitMethod(method)
    }
    cw.visitEnd()

    JVMClass(clazz.name, cw.toByteArray)
  }

  /**
    * Emit bytecode for a method. Code will be appended to the class writer.
    *
    * @param method the method
    * @param cw     the current class writer
    */
  def emitMethod(method: MethodDef)(implicit cw: ClassWriter): Unit = {
    // Methods are always public, but they can be static or not.
    val access = Opcodes.ACC_PUBLIC + (if (method.isStatic) Opcodes.ACC_STATIC else 0)
    val desc = if (method.symbol.isMain) MAIN_DESCRIPTOR else descriptor(method.symbol)
    implicit val mv = cw.visitMethod(access, method.name, desc, null, null)

    // Allocate indexes (in JVM local variable array) for every argument. For member methods, index 0 is reserved for
    // `this`.
    implicit val ctx = new Context(method.isStatic)
    method.params.foreach { p => ctx.declare(p.symbol) }

    // Visit method body and emit bytecode.
    mv.visitCode()
    emitStmt(method.body)
    appendReturnIfNecessary(method)
    mv.visitMaxs(-1, -1)
    mv.visitEnd()
  }

  /**
    * JVM requires every method to have explicit RETURN instruction for every execution path. For methods that actually
    * returns a value, our compiler frontend should ensure this. However, a decaf program may omit redundant return
    * statements in a method that returns nothing (or, returns void). In these conditions, we shall insert a RETURN
    * instruction at the end, if not any.
    *
    * @param methodDef the method
    * @param mv        the current method visitor
    */
  def appendReturnIfNecessary(methodDef: MethodDef)(implicit mv: MethodVisitor): Unit = {
    if (!methodDef.returnType.typ.isVoidType) return

    val stmts = methodDef.body.asInstanceOf[Block].stmts
    if (stmts.isEmpty || !stmts.last.isInstanceOf[Return]) mv.visitInsn(Opcodes.RETURN)
  }

  type LocalVars = mutable.HashMap[LocalVarSymbol, Int]

  class Context(isStatic: Boolean = true) {
    val index: LocalVars = new LocalVars

    def declare(v: LocalVarSymbol): Int = {
      index(v) = next
      val i = next
      next += 1
      i
    }

    private var next: Int = if (isStatic) 0 else 1
  }

  /**
    * Emit bytecode for a statement. Code will be appended to the method writer.
    *
    * @param stmt      the statement
    * @param mv        the current method writers
    * @param loopExits exit labels for the entered loops so far, arranged from the inner most to the outer most
    * @param ctx       the current context
    */
  def emitStmt(stmt: Stmt)(implicit mv: MethodVisitor, loopExits: List[Label] = Nil,
                           ctx: Context): Unit = stmt match {
    case Block(stmts) => stmts foreach emitStmt

    case v: LocalVarDef =>
      // JVM will complain if a local variable is read but not initialized yet. It also seems that when the local
      // variable is firstly initialized in a more inner scope rather than the outer most local scope, JVM reports
      // an error. To avoid these, let's simply initialize every user-defined local variable right now.
      val index = ctx.declare(v.symbol)
      loadDefaultValue(v.typeLit.typ)
      mv.visitVarInsn(storeOp(v.typeLit.typ), index)

    case Assign(lhs, rhs) =>
      lhs match {
        case LocalVar(v) =>
          emitExpr(rhs)
          mv.visitVarInsn(storeOp(v.typ), ctx.index(v))
        case MemberVar(receiver, v) =>
          emitExpr(receiver)
          emitExpr(rhs)
          mv.visitFieldInsn(Opcodes.PUTFIELD, internalName(v.owner), v.name, descriptor(v))
        case IndexSel(array, index) =>
          emitExpr(array)
          emitExpr(index)
          emitExpr(rhs)
          val elemType = array.typ.asInstanceOf[ArrayType].elemType
          mv.visitInsn(arrayStoreOp(elemType))
      }
    case ExprEval(expr) => emitExpr(expr)
    case Skip() => // nop

    case If(cond, trueBranch, falseBranch) =>
      emitExpr(cond)
      ifThenElse(emitStmt(trueBranch), emitStmt(falseBranch))
    case While(cond, body) =>
      val exit = new Label
      loop(emitExpr(cond), exit) { emitStmt(body)(mv, exit :: loopExits, ctx) }
    case For(init, cond, update, body) =>
      val exit = new Label
      emitStmt(init)
      loop(emitExpr(cond), exit) { emitStmt(body)(mv, exit :: loopExits, ctx); emitStmt(update) }
    case Break() => mv.visitJumpInsn(Opcodes.GOTO, loopExits.head)
    case Return(None) => mv.visitInsn(Opcodes.RETURN)
    case Return(Some(expr)) =>
      emitExpr(expr)
      mv.visitInsn(returnOp(expr.typ))
    case Print(exprs) =>
      exprs.foreach { expr => printing(emitExpr(expr), expr.typ) }
  }

  /**
    * Emit bytecode for an expression. Code will be appended to the method writer.
    *
    * @param expr the expression
    * @param mv   the current method writer
    * @param ctx  the current context
    */
  def emitExpr(expr: Expr)(implicit mv: MethodVisitor, ctx: Context): Unit = expr match {
    case IntLit(v) => mv.visitLdcInsn(v)
    case BoolLit(v) => mv.visitLdcInsn(v)
    case StringLit(v) => mv.visitLdcInsn(v)
    case NullLit() => mv.visitInsn(Opcodes.ACONST_NULL)

    // Prebuilt functions
    case ReadInt() => ???
    case ReadLine() => ???

    // Unary expressions
    case UnaryExpr(op, expr) =>
      emitExpr(expr)
      mv.visitInsn(unaryOp(op))

    // Binary expressions
    case BinaryExpr(op, lhs, rhs) =>
      emitExpr(lhs)
      emitExpr(rhs)
      op match {
        case op: ArithOp => mv.visitInsn(arithOp(op))
        case TreeNode.AND => mv.visitInsn(Opcodes.LAND)
        case TreeNode.OR => mv.visitInsn(Opcodes.LOR)
        case op: EqOrCmpOp => compare(op, lhs.typ)
      }

    // Local variables: they must be already assigned to an index
    case LocalVar(v) => mv.visitVarInsn(loadOp(v.typ), ctx.index(v))

    // Array related
    case NewArray(elemType, len) =>
      emitExpr(len)
      elemType.typ match {
        case t: JNative => mv.visitIntInsn(Opcodes.NEWARRAY, arrayTypeCode(t))
        case t => mv.visitTypeInsn(Opcodes.ANEWARRAY, toASMType(t).getInternalName)
      }
    case IndexSel(array, index) =>
      emitExpr(array)
      emitExpr(index)
      val elemType = array.typ.asInstanceOf[ArrayType].elemType
      mv.visitInsn(arrayLoadOp(elemType))
    case ArrayLen(array) =>
      emitExpr(array)
      mv.visitInsn(Opcodes.ARRAYLENGTH)

    // Class related
    case NewClass(clazz) =>
      mv.visitTypeInsn(Opcodes.NEW, internalName(clazz))
      mv.visitInsn(Opcodes.DUP)
      mv.visitMethodInsn(Opcodes.INVOKESPECIAL, internalName(clazz), CONSTRUCTOR_NAME, CONSTRUCTOR_DESC, false)
    case This() => mv.visitVarInsn(Opcodes.ALOAD, 0)
    case MemberVar(receiver, v) =>
      emitExpr(receiver)
      mv.visitFieldInsn(Opcodes.GETFIELD, internalName(v.owner), v.name, descriptor(v))
    case StaticCall(m, args) =>
      args foreach emitExpr
      mv.visitMethodInsn(Opcodes.INVOKESTATIC, internalName(m.owner), m.name, descriptor(m), false)
    case MemberCall(receiver, m, args) =>
      (receiver :: args) foreach emitExpr
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, internalName(m.owner), m.name, descriptor(m), false)

    case ClassTest(obj, clazz) =>
      emitExpr(obj)
      mv.visitTypeInsn(Opcodes.INSTANCEOF, internalName(clazz))
    case ClassCast(obj, clazz) =>
      emitExpr(obj)
      mv.visitTypeInsn(Opcodes.CHECKCAST, internalName(clazz))
  }

  override def post(output: List[JVMClass])(implicit opt: Config): Unit = {
    output.foreach { _.writeFile() }
  }
}
