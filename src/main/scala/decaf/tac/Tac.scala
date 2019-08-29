package decaf.tac

/**
  * Three Address Code. An Intermediate Representation (IR) as the input language of decaf's MIPS backend.
  */
object Tac {

  /**
    * A top-level TAC program consists of a list of virtual tables and procedures.
    */
  case class Program(vtbls: List[VTable], procs: List[Proc])

  /**
    * Instruction.
    */
  trait Instr

  // Assignment
  case class Move(dst: Temp, src: Temp) extends Instr

  case class LoadVTbl(dst: Temp, vtbl: VTable) extends Instr

  case class LoadImm4(dst: Temp, int: ConstTemp) extends Instr

  case class LoadStrConst(dst: Temp, str: String) extends Instr

  // Arithmetic
  case class Add(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Sub(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Mul(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Div(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Mod(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Neg(dst: Temp, src: Temp) extends Instr

  // Logical & Compare
  case class Equ(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Neq(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Les(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Leq(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Gtr(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class Geq(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class LAnd(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class LOr(dst: Temp, src1: Temp, src2: Temp) extends Instr

  case class LNot(dst: Temp, src: Temp) extends Instr

  // Control flow
  case class Branch(lbl: Label) extends Instr

  case class BEqZ(cond: Temp, lbl: Label) extends Instr

  case class BNeZ(cond: Temp, lbl: Label) extends Instr

  case class Ret(src: Temp = null) extends Instr

  // Procedure call
  case class Parm(src: Temp) extends Instr

  case class IndirectCall(dst: Temp, proc: Temp) extends Instr

  case class DirectCall(dst: Temp, proc: Label) extends Instr

  // Memory access
  case class Load(dst: Temp, base: Temp, offset: ConstTemp) extends Instr

  case class Store(src: Temp, base: Temp, offset: ConstTemp) extends Instr

  // Others
  case class Mark(lbl: Label) extends Instr

  case class Memo(memo: String = "") extends Instr

  /**
    * Instruction sequence, i.e. a list of instructions.
    */
  type InstrSeq = List[Instr]

  implicit def oneInstrAsSingletonInstrSeq(ins: Instr): InstrSeq = List(ins)
}
