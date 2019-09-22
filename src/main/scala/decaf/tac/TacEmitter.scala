package decaf.tac


import decaf.annot.SymbolImplicit._
import decaf.annot.TypeImplicit._
import decaf.annot.{BoolType, IntType, LocalVarSymbol, StringType}
import decaf.lowlevel.StringUtils
import decaf.lowlevel.instr.Temp
import decaf.lowlevel.label.Label
import decaf.lowlevel.tac.{FuncVisitor, Intrinsic, RuntimeError, TacInstr}
import decaf.tree.TreeNode
import decaf.tree.TypedTree._
import decaf.util.Conversions._

import scala.collection.mutable

trait TacEmitter {

  class Context {
    var vars = new mutable.TreeMap[LocalVarSymbol, Temp]
  }

  def emitStmt(stmt: Stmt)(implicit ctx: Context, loopExits: List[Label], fv: FuncVisitor): Unit = {
    stmt match {
      case Block(stmts) => stmts.foreach(emitStmt)

      case v: LocalVarDef =>
        val t = fv.freshTemp
        ctx.vars(v.symbol) = t
        v.init.foreach { expr =>
          val et = emitExpr(expr)
          fv.visitAssign(t, et)
        }

      case Assign(IndexSel(array, index), rhs) =>
        val at = emitExpr(array)
        val it = emitExpr(index)
        val addr = emitArrayElementAddress(at, it)
        val t = emitExpr(rhs)
        fv.visitStoreTo(addr, t)
      case Assign(MemberVar(receiver, v), rhs) =>
        val rt = emitExpr(receiver)
        val t = emitExpr(rhs)
        fv.visitMemberWrite(rt, v.owner.name, v.name, t)
      case Assign(LocalVar(v), rhs) =>
        val t = emitExpr(rhs)
        fv.visitAssign(ctx.vars(v), t)

      case ExprEval(expr) => emitExpr(expr)

      case If(cond, trueBranch, falseBranch) =>
        val t = emitExpr(cond)
        falseBranch match {
          case Some(f) => emitIfThenElse(t, emitStmt(trueBranch), emitStmt(f))
          case None => emitIfThen(t, emitStmt(trueBranch))
        }

      case While(cond, body) =>
        val exit = fv.freshLabel
        emitWhile(emitExpr(cond), emitStmt(body)(ctx, exit :: loopExits, fv), exit)
      case For(init, cond, update, body) =>
        val exit = fv.freshLabel
        emitStmt(init)
        emitWhile(emitExpr(cond), {
          emitStmt(body)(ctx, exit :: loopExits, fv)
          emitStmt(update)
        }, exit)
      case Break() => fv.visitBranch(loopExits.head)

      case Return(None) => fv.visitReturn()
      case Return(Some(expr)) =>
        val t = emitExpr(expr)
        fv.visitReturn(t)

      case Print(exprs) => exprs.foreach {
        case e if e.typ === IntType =>
          val t = emitExpr(e)
          fv.visitIntrinsicCall(Intrinsic.PRINT_INT, t)
        case e if e.typ === BoolType =>
          val t = emitExpr(e)
          fv.visitIntrinsicCall(Intrinsic.PRINT_BOOL, t)
        case e if e.typ === StringType =>
          val t = emitExpr(e)
          fv.visitIntrinsicCall(Intrinsic.PRINT_STRING, t)
      }
    }
  }

  def emitExpr(expr: Expr)(implicit ctx: Context, fv: FuncVisitor): Temp = {
    expr match {
      case IntLit(value) => fv.visitLoad(value)
      case BoolLit(value) => fv.visitLoad(value)
      case StringLit(value) => fv.visitLoad(StringUtils.unquote(value))
      case NullLit() => fv.visitLoad(0)

      case ReadInt() => fv.visitIntrinsicCall(Intrinsic.READ_INT, true)
      case ReadLine() => fv.visitIntrinsicCall(Intrinsic.READ_LINE, true)

      case LocalVar(v) => ctx.vars(v)

      case Unary(op, operand) =>
        val opcode = op match {
          case TreeNode.NEG => TacInstr.Unary.Op.NEG
          case TreeNode.NOT => TacInstr.Unary.Op.LNOT
        }
        val t = emitExpr(operand)
        fv.visitUnary(opcode, t)

      case Binary(op, lhs, rhs) if (op == TreeNode.EQ || op == TreeNode.NE) && lhs.typ === StringType =>
        // string eq/ne
        val lt = emitExpr(lhs)
        val rt = emitExpr(rhs)
        val result = fv.visitIntrinsicCall(Intrinsic.STRING_EQUAL, true, lt, rt)
        if (op == TreeNode.NE) {
          fv.visitUnarySelf(TacInstr.Unary.Op.LNOT, result)
        }
        result

      case Binary(op, lhs, rhs) =>
        import decaf.lowlevel.tac.TacInstr
        val opcode = op match {
          case TreeNode.ADD => TacInstr.Binary.Op.ADD
          case TreeNode.SUB => TacInstr.Binary.Op.SUB
          case TreeNode.MUL => TacInstr.Binary.Op.MUL
          case TreeNode.DIV => TacInstr.Binary.Op.DIV
          case TreeNode.MOD => TacInstr.Binary.Op.MOD
          case TreeNode.EQ => TacInstr.Binary.Op.EQU
          case TreeNode.NE => TacInstr.Binary.Op.NEQ
          case TreeNode.LT => TacInstr.Binary.Op.LES
          case TreeNode.LE => TacInstr.Binary.Op.LEQ
          case TreeNode.GT => TacInstr.Binary.Op.GTR
          case TreeNode.GE => TacInstr.Binary.Op.GEQ
          case TreeNode.AND => TacInstr.Binary.Op.LAND
          case TreeNode.OR => TacInstr.Binary.Op.LOR
        }

        val lt = emitExpr(lhs)
        val rt = emitExpr(rhs)
        fv.visitBinary(opcode, lt, rt)

      case NewArray(_, length) =>
        val lt = emitExpr(length)
        emitArrayInit(lt)
      case IndexSel(array, index) =>
        val at = emitExpr(array)
        val it = emitExpr(index)
        val addr = emitArrayElementAddress(at, it)
        fv.visitLoadFrom(addr)
      case ArrayLen(array) =>
        val at = emitExpr(array)
        fv.visitLoadFrom(at, -4)

      case NewClass(clazz) => fv.visitNewClass(clazz.name)
      case This() => fv.getArgTemp(0)
      case MemberVar(receiver, field) =>
        val rt = emitExpr(receiver)
        fv.visitMemberAccess(rt, field.owner.name, field.name)

      case MemberCall(receiver, method, args) =>
        val rt = emitExpr(receiver)
        val as = args.map(emitExpr)
        if (method.typ.ret.isVoidType) {
          fv.visitMemberCall(rt, method.owner.name, method.name, as)
          null
        } else fv.visitMemberCall(rt, method.owner.name, method.name, as, true)
      case StaticCall(method, args) =>
        val as = args.map(emitExpr)
        if (method.typ.ret.isVoidType) {
          fv.visitStaticCall(method.owner.name, method.name, as)
          null
        } else fv.visitStaticCall(method.owner.name, method.name, as, true)

      case ClassTest(obj, clazz) if obj.typ <= clazz.typ =>
        // Accelerate: when obj.type <: is.type, the test must be successful!
        fv.visitLoad(true)
      case ClassTest(obj, clazz) =>
        val ot = emitExpr(obj)
        emitClassTest(ot, clazz.name)
      case ClassCast(obj, clazz) if obj.typ <= clazz.typ =>
        // Accelerate: when obj.type <: is.type, the test must be successful!
        emitExpr(obj)
      case ClassCast(obj, clazz) =>
        val ot = emitExpr(obj)
        val result = emitClassTest(ot, clazz.name)
        /* Pseudo code:
         * {{{
         *     if (result != 0) branch exit  // cast success
         *     print "Decaf runtime error: " // RuntimeError.CLASS_CAST_ERROR1
         *     vtbl1 = *obj                  // vtable of obj
         *     fromClass = *(vtbl1 + 4)      // name of obj's class
         *     print fromClass
         *     print " cannot be cast to "   // RuntimeError.CLASS_CAST_ERROR2
         *     vtbl2 = load vtbl of the target class
         *     toClass = *(vtbl2 + 4)        // name of target class
         *     print toClass
         *     print "\n"                    // RuntimeError.CLASS_CAST_ERROR3
         *     halt
         * exit:
         * }}}
         */
        val exit = fv.freshLabel
        fv.visitBranch(TacInstr.CondBranch.Op.BNEZ, result, exit)
        fv.visitPrint(RuntimeError.CLASS_CAST_ERROR1)
        val vtbl1 = fv.visitLoadFrom(ot)
        val fromClass = fv.visitLoadFrom(vtbl1, 4)
        fv.visitIntrinsicCall(Intrinsic.PRINT_STRING, fromClass)
        fv.visitPrint(RuntimeError.CLASS_CAST_ERROR2)
        val vtbl2 = fv.visitLoadVTable(clazz.name)
        val toClass = fv.visitLoadFrom(vtbl2, 4)
        fv.visitIntrinsicCall(Intrinsic.PRINT_STRING, toClass)
        fv.visitPrint(RuntimeError.CLASS_CAST_ERROR3)
        fv.visitIntrinsicCall(Intrinsic.HALT)
        fv.visitLabel(exit)
        ot
    }
  }

  /**
    * Emit code for the following conditional statement:
    * {{{
    * if (cond) {
    * action
    * }
    * }}}
    * Implementation in pseudo code:
    * {{{
    * if (cond == 0) branch skip;
    * action
    * skip:
    * }}}
    *
    * @param cond   temp of condition
    * @param action code (to be generated) of the true branch
    * @param mv     current method visitor
    */
  private def emitIfThen(cond: Temp, action: => Unit)(implicit mv: FuncVisitor): Unit = {
    val skip = mv.freshLabel
    mv.visitBranch(TacInstr.CondBranch.Op.BEQZ, cond, skip)
    action
    mv.visitLabel(skip)
  }

  /**
    * Emit code for the following conditional statement:
    * {{{
    * if (cond) {
    * trueBranch
    * } else {
    * falseBranch
    * }
    * }}}
    * Implementation in pseudo code:
    * <pre>
    * if (cond == 0) branch skip
    * trueBranch
    * branch exit
    * skip:
    * falseBranch
    * exit:
    * </pre>
    *
    * @param cond        temp of condition
    * @param trueBranch  code (to be generated) of the true branch
    * @param falseBranch code (to be generated) of the false branch
    * @param mv          current method visitor
    */
  private def emitIfThenElse(cond: Temp, trueBranch: => Unit, falseBranch: => Unit)(implicit mv: FuncVisitor): Unit = {
    val skip = mv.freshLabel
    val exit = mv.freshLabel
    mv.visitBranch(TacInstr.CondBranch.Op.BEQZ, cond, skip)
    trueBranch
    mv.visitBranch(exit)
    mv.visitLabel(skip)
    falseBranch
    mv.visitLabel(exit)
  }

  /**
    * Emit code for the following loop:
    * <pre>
    * while (cond) {
    * block
    * }
    * </pre>
    * <p>
    * Implementation in pseudo code:
    * <pre>
    * entry:
    * cond = do test
    * if (cond == 0) branch exit
    * do block
    * branch entry
    * exit:
    * </pre>
    *
    * @param test  code (to be generated) of the loop condition
    * @param block code (to be generated) of the loop body
    * @param exit  label of loop exit
    * @param mv    current method visitor
    */
  private def emitWhile(test: => Temp, block: => Unit, exit: Label)(implicit mv: FuncVisitor): Unit = {
    val entry = mv.freshLabel
    mv.visitLabel(entry)
    val cond = test
    mv.visitBranch(TacInstr.CondBranch.Op.BEQZ, cond, exit)
    block
    mv.visitBranch(entry)
    mv.visitLabel(exit)
  }

  /**
    * Emit code for initializing a new array.
    * <p>
    * In memory, an array of length {@code n} takes {@code (n + 1) * 4} bytes:
    * - the first 4 bytes: length
    * - the rest bytes: data
    * <p>
    * Pseudo code:
    * <pre>
    * error = length < 0
    * if (error) {
    * throw RuntimeError.NEGATIVE_ARR_SIZE
    * }
    *
    * units = length + 1
    * size = units * 4
    * a = ALLOCATE(size)
    * *(a + 0) = length
    * p = a + size
    * p -= 4
    * while (p != a) {
    * *(p + 0) = 0
    * p -= 4
    * }
    * ret = (a + 4)
    * </pre>
    *
    * @param length temp of array length
    * @param mv     current method visitor
    * @return a temp storing the address of the first element of the array
    */
  private def emitArrayInit(length: Temp)(implicit mv: FuncVisitor): Temp = {
    val zero = mv.visitLoad(0)
    emitIfThen(mv.visitBinary(TacInstr.Binary.Op.LES, length, zero), {
      mv.visitPrint(RuntimeError.NEGATIVE_ARR_SIZE)
      mv.visitIntrinsicCall(Intrinsic.HALT)
    })
    val units = mv.visitBinary(TacInstr.Binary.Op.ADD, length, mv.visitLoad(1))
    val four = mv.visitLoad(4)
    val size = mv.visitBinary(TacInstr.Binary.Op.MUL, units, four)
    val a = mv.visitIntrinsicCall(Intrinsic.ALLOCATE, true, size)
    mv.visitStoreTo(a, length)
    val p = mv.visitBinary(TacInstr.Binary.Op.ADD, a, size)
    mv.visitBinarySelf(TacInstr.Binary.Op.SUB, p, four)
    emitWhile(mv.visitBinary(TacInstr.Binary.Op.NEQ, p, a), {
      mv.visitStoreTo(p, zero)
      mv.visitBinarySelf(TacInstr.Binary.Op.SUB, p, four)
    }, mv.freshLabel)
    mv.visitBinary(TacInstr.Binary.Op.ADD, a, four)
  }

  /**
    * Emit code for computing the address of an array element.
    * <p>
    * Pseudo code:
    * <pre>
    * length = *(array - 4)
    * error1 = index {@literal <} 0
    * error2 = index {@literal >=} length
    * error = error1 || error2
    * if (error) {
    * throw RuntimeError.ARRAY_INDEX_OUT_OF_BOUND
    * }
    *
    * offset = index * 4
    * ret = array + offset
    * </pre>
    *
    * @param array temp of the array
    * @param index temp of the index
    * @return a temp storing the address of the element
    */
  private def emitArrayElementAddress(array: Temp, index: Temp)(implicit mv: FuncVisitor): Temp = {
    val length = mv.visitLoadFrom(array, -4)
    val zero = mv.visitLoad(0)
    val error1 = mv.visitBinary(TacInstr.Binary.Op.LES, index, zero)
    val error2 = mv.visitBinary(TacInstr.Binary.Op.GEQ, index, length)
    emitIfThen(mv.visitBinary(TacInstr.Binary.Op.LOR, error1, error2), {
      mv.visitPrint(RuntimeError.ARRAY_INDEX_OUT_OF_BOUND)
      mv.visitIntrinsicCall(Intrinsic.HALT)
    })
    val four = mv.visitLoad(4)
    val offset = mv.visitBinary(TacInstr.Binary.Op.MUL, index, four)
    mv.visitBinary(TacInstr.Binary.Op.ADD, array, offset)
  }

  /**
    * Emit code for testing if an object is an instance of class.
    * <p>
    * Pseudo code:
    * <pre>
    * target = LoadVtbl(clazz)
    * t = *object
    * loop:
    * ret = t == target
    * if (ret != 0) goto exit
    * t = *t
    * if (t != 0) goto loop
    * ret = 0 // t == null
    * exit:
    * </pre>
    *
    * @param obj   temp of the object/instance
    * @param clazz name of the class
    * @return a temp storing the result (1 for true, and 0 for false)
    */
  private def emitClassTest(obj: Temp, clazz: String)(implicit mv: FuncVisitor): Temp = {
    val target = mv.visitLoadVTable(clazz)
    val t = mv.visitLoadFrom(obj)
    val loop = mv.freshLabel
    val exit = mv.freshLabel
    mv.visitLabel(loop)
    val ret = mv.visitBinary(TacInstr.Binary.Op.EQU, t, target)
    mv.visitBranch(TacInstr.CondBranch.Op.BNEZ, ret, exit)
    mv.visitRaw(new TacInstr.Memory(TacInstr.Memory.Op.LOAD, t, t, 0))
    mv.visitBranch(TacInstr.CondBranch.Op.BNEZ, t, loop)
    val zero = mv.visitLoad(0)
    mv.visitAssign(ret, zero)
    mv.visitLabel(exit)
    ret
  }
}
