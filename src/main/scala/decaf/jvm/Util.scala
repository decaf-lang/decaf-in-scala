package decaf.jvm

import decaf.frontend.annot._
import decaf.frontend.tree.TreeNode
import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type => ASMType}

/**
  * Utilities for emitting JVM bytecode.
  */
trait Util {

  /**
    * Internal name of Java's super class -- the well-known `java.lang.Object`.
    */
  val JAVA_SUPER_INTERNAL_NAME = ASMType.getInternalName(classOf[java.lang.Object])

  /**
    * Default name of constructors.
    */
  val CONSTRUCTOR_NAME = "<init>"

  /**
    * Type descriptor of constructors.
    */
  val CONSTRUCTOR_DESC = ASMType.getMethodDescriptor(ASMType.VOID_TYPE)

  /**
    * Type descriptor of main method.
    */
  val MAIN_DESCRIPTOR: String = "([Ljava/lang/String;)V"

  /**
    * Emit bytecode for if-then-else branching. The boolean value of condition shall now be on the stack top.
    * Pseudo code:
    * {{{
    *     if != 0 goto true
    *     falseBranch
    *     goto exit
    *   true:
    *     trueBranch
    *   exit:
    * }}}
    *
    * @param trueBranch  code (to be generated) of the true branch
    * @param falseBranch code (to be generated) of the true branch
    * @param mv          method visitor
    */
  def ifThenElse(trueBranch: => Unit, falseBranch: => Unit)(implicit mv: MethodVisitor): Unit = {
    val trueLabel = new Label
    val exitLabel = new Label

    mv.visitJumpInsn(Opcodes.IFNE, trueLabel)
    falseBranch
    mv.visitJumpInsn(Opcodes.GOTO, exitLabel)
    mv.visitLabel(trueLabel)
    trueBranch
    mv.visitLabel(exitLabel)
  }

  /**
    * Emit bytecode for while-loop.
    * Pseudo code:
    * {{{
    *   enter:
    *     cond
    *     if = 0 goto exit
    *     body
    *     goto enter
    *   exit:
    * }}}
    *
    * @param cond code (to be generated) for evaluating the condition
    * @param body code (to be generated) of the loop body
    * @param exit label of loop exit
    * @param mv   method visitor
    */
  def loop(cond: => Unit, exit: Label)(body: => Unit)(implicit mv: MethodVisitor): Unit = {
    val enter = new Label

    mv.visitLabel(enter)
    cond
    mv.visitJumpInsn(Opcodes.IFEQ, exit)
    body
    mv.visitJumpInsn(Opcodes.GOTO, enter)
    mv.visitLabel(exit)
  }

  /**
    * Emit bytecode for calling I/O methods of java.util.Scanner, as illustrated by the following scala code:
    * {{{
    *   val in = new java.util.Scanner(System.in)
    *   in.<method>()
    * }}}
    *
    * @param method the member method of java.util.Scanner, i.e. `nextInt` or `nextLine`
    * @param mv     the current method visitor
    */
  def callScanner(method: String)(implicit mv: MethodVisitor): Unit = {
    val system = classOf[System]
    val system_in = system.getDeclaredField("in").getType
    val scanner = classOf[java.util.Scanner]

    mv.visitTypeInsn(Opcodes.NEW, ASMType.getInternalName(scanner))
    mv.visitInsn(Opcodes.DUP)
    mv.visitFieldInsn(Opcodes.GETSTATIC, ASMType.getInternalName(system), "in",
      ASMType.getDescriptor(system_in))
    mv.visitMethodInsn(Opcodes.INVOKESPECIAL, ASMType.getInternalName(scanner), CONSTRUCTOR_NAME,
      ASMType.getConstructorDescriptor(scanner.getDeclaredConstructor(system_in)), false)
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, ASMType.getInternalName(scanner), method,
      ASMType.getMethodDescriptor(scanner.getDeclaredMethod(method)), false)
  }

  /**
    * Emit bytecode for calling System.out.print, as illustrated by the following scala code:
    * {{{
    *   val str = String.valueOf(arg)
    *   System.out.print(str)
    * }}}
    *
    * @param arg the bytecode which computes the value of the argument
    * @param typ the type of the argument, should be int/bool/string
    * @param mv  the current method visitor
    */
  def printing(arg: => Unit, typ: Type)(implicit mv: MethodVisitor): Unit = {
    val system = classOf[System]
    val system_out = system.getDeclaredField("out").getType
    val string = classOf[java.lang.String]

    mv.visitFieldInsn(Opcodes.GETSTATIC, ASMType.getInternalName(system), "out", ASMType.getDescriptor(system_out))
    arg
    typ match {
      case IntType =>
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, ASMType.getInternalName(string), "valueOf",
          ASMType.getMethodDescriptor(string.getDeclaredMethod("valueOf", classOf[Int])), false)
      case BoolType =>
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, ASMType.getInternalName(string), "valueOf",
          ASMType.getMethodDescriptor(string.getDeclaredMethod("valueOf", classOf[Boolean])), false)
      case StringType => // nop
    }
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, ASMType.getInternalName(system_out), "print",
      ASMType.getMethodDescriptor(system_out.getDeclaredMethod("print", string)), false)
  }

  /** Translate a Decaf type to JVM assembly type. */
  def toASMType(typ: Type): ASMType = typ match {
    case IntType => ASMType.INT_TYPE
    case BoolType => ASMType.BOOLEAN_TYPE
    case StringType => ASMType.getType(classOf[java.lang.String])
    case VoidType => ASMType.VOID_TYPE
    case ClassType(name, _) => ASMType.getObjectType(name)
    case ArrayType(elemType) => ASMType.getType('[' + toASMType(elemType).getDescriptor)
    case FunType(params, ret) => ASMType.getMethodType(toASMType(ret), params.map(toASMType): _*)
  }

  /**
    * Get the internal name of a class symbol.
    *
    * @param clazz the class symbol
    * @return its internal name
    */
  def internalName(clazz: ClassSymbol): String = toASMType(clazz.typ).getInternalName

  /**
    * Get the (type) descriptor of a field symbol.
    * See https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3 for descriptor syntax.
    *
    * @param field the field symbol, i.e. member var/method
    * @return its descriptor
    */
  def descriptor(field: FieldSymbol): String = toASMType(field.typ).getDescriptor

  // -----------------------------------------------------------------------------------------------
  // The following group of methods handle selection choice based on types. Since JVM has NO useful
  // operations on booleans directly, by default we always regard a boolean value as an integer:
  // 1 for true and 0 for false. In this way, the I-instructions are suitable for int and bool,
  // and A-instructions for reference types (like objects and arrays).
  //
  // The only exception is that, boolean arrays are encoded as byte arrays, i.e. each element takes
  // 8 bits. In this way, we must use the BA-instructions to load and store value from byte arrays.
  //
  // I believe the above convention is consistent with the implementation of the Java language.
  //
  // NOTE: every time you encounter the following runtime error:
  // java.lang.NegativeArraySizeException: -1
  //     at org.objectweb.asm.Frame.merge(Frame.java:1222)
  // It is possible that your instructions have popped more bytes than the stack has. For instance,
  // you attempt to pop a long (32 bit) from the stack, given that you just pushed an int (16 bit).
  // -----------------------------------------------------------------------------------------------

  def loadDefaultValue(typ: Type)(implicit mv: MethodVisitor): Unit = typ match {
    case IntType | BoolType => mv.visitInsn(Opcodes.ICONST_0)
    case _ => mv.visitInsn(Opcodes.ACONST_NULL)
  }

  def loadOp(typ: Type): Int = typ match {
    case IntType | BoolType => Opcodes.ILOAD
    case _ => Opcodes.ALOAD
  }

  def storeOp(typ: Type): Int = typ match {
    case IntType | BoolType => Opcodes.ISTORE
    case _ => Opcodes.ASTORE
  }

  def returnOp(typ: Type): Int = typ match {
    case IntType | BoolType => Opcodes.IRETURN
    case _ => Opcodes.ARETURN
  }

  def arrayTypeCode(elemType: JNativeType): Int = elemType match {
    case IntType => Opcodes.T_INT
    case BoolType => Opcodes.T_BOOLEAN
  }

  def arrayLoadOp(elemType: Type): Int = elemType match {
    case IntType => Opcodes.IALOAD
    case BoolType => Opcodes.BALOAD
    case _ => Opcodes.AALOAD
  }

  def arrayStoreOp(elemType: Type): Int = elemType match {
    case IntType => Opcodes.IASTORE
    case BoolType => Opcodes.BASTORE
    case _ => Opcodes.AASTORE
  }

  def arithOp(op: TreeNode.ArithOp): Int = op match {
    case TreeNode.ADD => Opcodes.IADD
    case TreeNode.SUB => Opcodes.ISUB
    case TreeNode.MUL => Opcodes.IMUL
    case TreeNode.DIV => Opcodes.IDIV
    case TreeNode.MOD => Opcodes.IREM
  }

  /**
    * Emit bytecode for comparing two values `left` and `right` (already on stack).
    * Procedure:
    * {{{
    *     if (left ? right) goto true
    *     push 0
    *     goto exit
    *   true:
    *     push 1
    *   exit:
    * }}}
    *
    * For <, <=, >, >=, the two values must both have type int.
    * However, for == and !=, three cases need be handled separately:
    * - Values are both integers or booleans: compare their actual value by int instructions.
    * - Values are both strings: compare their actual value by invoking ???. // TODO
    * - Values are both arrays/objects: compare their address by reference instructions.
    */
  def compare(op: TreeNode.EqOrCmpOp, typ: Type)(implicit mv: MethodVisitor): Unit = {
    val trueLabel = new Label
    val exitLabel = new Label
    val opCode = op match {
      case TreeNode.EQ =>
        if (typ === IntType) Opcodes.IF_ICMPEQ else Opcodes.IF_ACMPEQ
      case TreeNode.NE =>
        if (typ === IntType) Opcodes.IF_ICMPNE else Opcodes.IF_ACMPNE
      case TreeNode.LE =>
        assert(typ === IntType)
        Opcodes.IF_ICMPLE
      case TreeNode.LT =>
        assert(typ === IntType)
        Opcodes.IF_ICMPLT
      case TreeNode.GE =>
        assert(typ === IntType)
        Opcodes.IF_ICMPGE
      case TreeNode.GT =>
        assert(typ === IntType)
        Opcodes.IF_ICMPGT
    }

    mv.visitJumpInsn(opCode, trueLabel)
    mv.visitInsn(Opcodes.ICONST_0)
    mv.visitJumpInsn(Opcodes.GOTO, exitLabel)
    mv.visitLabel(trueLabel)
    mv.visitInsn(Opcodes.ICONST_1)
    mv.visitLabel(exitLabel)
  }
}
