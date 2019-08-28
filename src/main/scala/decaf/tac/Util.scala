package decaf.tac

import decaf.error.RuntimeError
import decaf.tac.Lib.Intrinsic
import decaf.tac.Tac._
import decaf.tree.TreeNode

trait Util {

  class InstrBlock(val seq: InstrSeq = Nil) {
    def isEmpty: Boolean = seq.isEmpty

    def ||(block: InstrBlockValued): InstrBlockValued = block match {
      case InstrBlockValued(next, finalValue) => InstrBlockValued(seq ++ next, finalValue)
    }

    def ||(block: InstrBlock): InstrBlock = new InstrBlock(seq ++ block.seq)

    def returns(value: Temp): InstrBlockValued = InstrBlockValued(seq, value)
  }

  case class InstrBlockValued(override val seq: InstrSeq, value: Temp) extends InstrBlock(seq) {
    def >>(g: Temp => InstrBlockValued): InstrBlockValued = this || g(value)

    def >|(g: Temp => InstrBlock): InstrBlock = this || g(value)
  }

  implicit def __getValue__(self: InstrBlockValued): Temp = self.value

  implicit def __asNotValued__(seq: InstrSeq): InstrBlock = new InstrBlock(seq)

  implicit def __asNotValued__(instr: Instr): InstrBlock = new InstrBlock(List(instr))

  implicit def __asValued__(temp: Temp): InstrBlockValued = InstrBlockValued(Nil, temp)

  implicit def __asConstTemp__(int: Int): ConstTemp = ConstTemp(int)

  implicit def __asTempList__(temp: Temp): List[Temp] = List(temp)

  implicit def emit[T](f: (Temp, T) => Instr): T => InstrBlockValued = { x =>
    val t = Temp.fresh
    InstrBlockValued(f(t, x), t)
  }

  implicit def emit[T, U](f: (Temp, T, U) => Instr): (T, U) => InstrBlockValued = { (x, y) =>
    val t = Temp.fresh
    InstrBlockValued(f(t, x, y), t)
  }

  def binary(op: (Temp, Temp, Temp) => Instr, lhs: Temp): Temp => InstrBlockValued = rhs => emit(op)(lhs, rhs)

  def binary(op: (Temp, Temp, Temp) => Instr, lhs: Temp, rhs: Temp): InstrBlockValued = emit(op)(lhs, rhs)

  implicit class __Binary__(lhs: Temp) {
    def +(rhs: Temp): InstrBlockValued = binary(Add, lhs, rhs)

    def -(rhs: Temp): InstrBlockValued = binary(Sub, lhs, rhs)

    def *(rhs: Temp): InstrBlockValued = binary(Mul, lhs, rhs)

    def =?(rhs: Temp): InstrBlockValued = binary(Equ, lhs, rhs)

    def !=?(rhs: Temp): InstrBlockValued = binary(Neq, lhs, rhs)

    def <(rhs: Temp): InstrBlockValued = binary(Les, lhs, rhs)

    def <=(rhs: Temp): InstrBlockValued = binary(Leq, lhs, rhs)

    def >(rhs: Temp): InstrBlockValued = binary(Gtr, lhs, rhs)

    def >=(rhs: Temp): InstrBlockValued = binary(Geq, lhs, rhs)

    def or(rhs: Temp): InstrBlockValued = binary(LOr, lhs, rhs)
  }

  implicit def opToTac(op: TreeNode.Op): (Temp, Temp, Temp) => Instr = op match {
    case TreeNode.ADD => Add
    case TreeNode.SUB => Sub
    case TreeNode.MUL => Mul
    case TreeNode.DIV => Div
    case TreeNode.MOD => Mod
    case TreeNode.AND => LAnd
    case TreeNode.OR => LOr
    case TreeNode.EQ => Equ
    case TreeNode.NE => Neq
    case TreeNode.LT => Les
    case TreeNode.LE => Leq
    case TreeNode.GT => Gtr
    case TreeNode.GE => Geq
  }

  def load(value: Int): InstrBlockValued = emit(LoadImm4)(value)

  def load(value: String): InstrBlockValued = emit(LoadStrConst)(value)

  def load(base: Temp, offset: Int = 0): InstrBlockValued = emit(Load)(base, offset)

  def loadWith(offset: Int = 0)(base: Temp): InstrBlockValued = emit(Load)(base, offset)

  def ifFalseGoto(label: Label)(cond: Temp): InstrBlock = BEqZ(cond, label)

  /**
    * If condition:
    * {{{
    *   if (cond != 0) branch pass
    *   <body>
    *   pass:
    * }}}
    *
    * {{{
    *   if (cond) {
    *     <body>
    *   }
    * }}}
    *
    * @param body the body to be executed when the condition is not hold
    * @param cond the condition
    * @return an instruction block without return value
    */
  def ifFalseThen(body: InstrBlock)(cond: Temp): InstrBlock = {
    val pass = Label.fresh(true)
    BNeZ(cond, pass) || body || Mark(pass)
  }

  /**
    * {{{
    *     if (cond != 0) branch t
    *     <falseBranch>
    *     branch exit
    *   t:
    *     <trueBranch>
    *   exit:
    * }}}
    *
    * @return
    */
  def ifThenElse(trueBranch: InstrBlock, falseBranch: InstrBlock)(cond: Temp): InstrBlock = {
    val t = Label.fresh()
    val exit = Label.fresh()
    BNeZ(cond, t) || falseBranch || Branch(exit) || Mark(t) || trueBranch || Mark(exit)
  }

  def ifThen(branch: InstrBlock)(cond: Temp): InstrBlock = ifThenElse(branch, Nil)(cond)

  def loop(cond: InstrBlockValued, exit: Label = Label.fresh())(body: InstrBlock): InstrBlock = {
    val enter = Label.fresh()
    Mark(enter) || cond >| ifFalseGoto(exit) || body || Branch(enter) || Mark(exit)
  }

  def directCall(fun: Label, args: List[Temp] = Nil): InstrBlockValued = {
    val parm = new InstrBlock(args.map(Parm))
    val ret = Temp.fresh
    parm || DirectCall(ret, fun) returns ret
  }

  def indirectCall(args: List[Temp] = Nil)(fun: Temp): InstrBlockValued = {
    val parm = new InstrBlock(args.map(Parm))
    val ret = Temp.fresh
    parm || IndirectCall(ret, fun) returns ret
  }

  def intrinsicCall(intrinsic: Intrinsic, args: Temp*): InstrBlockValued = directCall(intrinsic.label, args.toList)

  def printString(temp: Temp): InstrBlockValued = intrinsicCall(Lib.PRINT_STRING, temp)

  def printString(str: String): InstrBlockValued = load(str) >> printString

  /**
    * Initialize array, set all bytes (except the first byte, i.e. length field) as 0.
    * {{{
    *   if (length < 0) {
    *     throw RuntimeError.NEGATIVE_ARR_SIZE
    *   }
    *
    *   size = (length + 1) * 4
    *   arr = ALLOCATE(size)
    *   *(arr + 0) = length
    *   ptr = arr + size
    *   ptr -= 4
    *   while (ptr != arr) {
    *     *(ptr + 0) = 0
    *     ptr -= 4
    *   }
    *   return (arr + 4)
    * }}}
    *
    * @param length a temp whose value is the array length
    * @return code with the starting address of the array (its first element)
    */
  def newArray(length: Temp): InstrBlockValued = {
    val zero = load(0)
    val word = load(WORD_SIZE)
    val checkLength = (length < zero) >| ifThen {
      printString(RuntimeError.NEGATIVE_ARR_SIZE) || intrinsicCall(Lib.HALT)
    }

    val size = load(1) >> (length + _) >> (_ * word)
    val arr = intrinsicCall(Lib.ALLOCATE, size)
    val ptr = arr.value + size
    val init = Store(length, arr, 0) || ptr || Sub(ptr, ptr, word) ||
      loop(ptr.value !=? arr) { Store(zero, ptr, 0) || Sub(ptr, ptr, word) }

    zero || word || checkLength || size || arr || init || (arr.value + word)
  }

  /**
    * Access array element by index.
    * {{{
    *   length = *(array - 4)
    *   if (index < 0 || index >= length) {
    *     throw RuntimeError.ARRAY_INDEX_OUT_OF_BOUND
    *   }
    *   return (index * 4 + array)
    * }}}
    *
    * @param array
    * @param index
    * @return the address of array index
    */
  def arrayElemRef(array: Temp, index: Temp): InstrBlockValued = {
    val cond1 = load(0) >> (index < _)
    val cond2 = load(array, -4) >> (index >= _)
    cond1 || cond2 || (cond1.value or cond2) >| ifThen {
      printString(RuntimeError.ARRAY_INDEX_OUT_OF_BOUND) || intrinsicCall(Lib.HALT)
    } || load(WORD_SIZE) >> (index * _) >> (_ + array)
  }

  def classTest(target: VTable)(obj: Temp): InstrBlockValued = {
    /**
      * targetVp = LoadVTbl(target)
      * vp = *obj
      * loop:
      * ret = (vp =? targetVp)
      * if (ret) goto exit
      * vp = *vp
      * if (vp) goto loop
      * ret = 0 // vp == null
      * exit: // return ret
      */
    val targetVp = emit(LoadVTbl)(target)
    val vp = load(obj)
    val loop = Label.fresh()
    val exit = Label.fresh()
    val ret = Temp.fresh
    targetVp || vp || Mark(loop) || Equ(ret, vp, targetVp) || BNeZ(ret, exit) || Load(vp, vp, 0) ||
      BNeZ(vp, loop) || LoadImm4(ret, 0) || Mark(exit) returns ret
  }

  def classCast(obj: Temp, target: VTable): InstrBlockValued = {
    /**
      * targetVp = LoadVTbl(target)
      * vp = *obj
      * loop:
      * ret = (vp =? targetVp)
      * if (ret) goto exit
      * vp = *vp
      * if (vp) goto loop
      * // handle error
      * exit:
      */
    val targetVp = emit(LoadVTbl)(target)
    val vp = load(obj)
    val loop = Label.fresh()
    val exit = Label.fresh()
    val ret = Temp.fresh
    val test = targetVp || vp || Mark(loop) || Equ(ret, vp, targetVp) || BNeZ(ret, exit) || Load(vp, vp, 0) ||
      BNeZ(vp, loop)

    val error = printString(RuntimeError.CLASS_CAST_ERROR1) ||
      load(obj) >> { load(_, 4) } >> printString ||
      printString(RuntimeError.CLASS_CAST_ERROR2) ||
      load(targetVp, 4) >> printString ||
      printString(RuntimeError.CLASS_CAST_ERROR3) || intrinsicCall(Lib.HALT)

    test || error || Mark(exit) returns obj
  }

  final val WORD_SIZE: Int =
    4
}
