package decaf.frontend.tac

import decaf.frontend.parsing.StringUtil

object Tac {

  /**
    * A top-level tac program consists of a list of virtual tables and functys.
    */
  case class Program(vtbls: List[VTable], procs: List[Proc]) {
    override def toString: String = (vtbls.map(_.toString) ++ procs.map(_.toString)).mkString("\n")
  }

  trait Instr

  // Assignment
  case class Move(dst: Temp, src: Temp) extends Instr {
    override def toString: String = s"$dst = $src"
  }

  case class LoadVTbl(dst: Temp, vtbl: VTable) extends Instr {
    override def toString: String = s"$dst = VTBL<${ vtbl.name }>"
  }

  case class LoadImm4(dst: Temp, int: ConstTemp) extends Instr {
    override def toString: String = s"$dst = $int"
  }

  case class LoadStrConst(dst: Temp, str: String) extends Instr {
    override def toString: String = s"$dst = ${ StringUtil.quote(str) }"
  }

  // Arithmetic
  case class Add(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 + $src2)"
  }

  case class Sub(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 - $src2)"
  }

  case class Mul(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 * $src2)"
  }

  case class Div(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 / $src2)"
  }

  case class Mod(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 % $src2)"
  }

  case class Neg(dst: Temp, src: Temp) extends Instr {
    override def toString: String = s"$dst = - $src"
  }

  // Logical & Compare
  case class Equ(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 == $src2)"
  }

  case class Neq(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 != $src2)"
  }

  case class Les(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 < $src2)"
  }

  case class Leq(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 <= $src2)"
  }

  case class Gtr(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 > $src2)"
  }

  case class Geq(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 >= $src2)"
  }

  case class LAnd(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 && $src2)"
  }

  case class LOr(dst: Temp, src1: Temp, src2: Temp) extends Instr {
    override def toString: String = s"$dst = ($src1 || $src2)"
  }

  case class LNot(dst: Temp, src: Temp) extends Instr {
    override def toString: String = s"$dst = ! $src"
  }

  // Control flow
  case class Branch(lbl: Label) extends Instr {
    override def toString: String = s"branch $lbl"
  }

  case class BEqZ(cond: Temp, lbl: Label) extends Instr {
    override def toString: String = s"if ($cond == 0) branch $lbl"
  }

  case class BNeZ(cond: Temp, lbl: Label) extends Instr {
    override def toString: String = s"if ($cond != 0) branch $lbl"
  }

  case class Ret(src: Temp) extends Instr {
    override def toString: String = "return " + (if (src == null) "<empty>" else src.toString)
  }

  // Procedure call
  case class Parm(src: Temp) extends Instr {
    override def toString: String = s"parm $src"
  }

  case class IndirectCall(dst: Temp, proc: Temp) extends Instr {
    override def toString: String = (if (dst != null) s"$dst = " else "") + s"call $proc"
  }

  case class DirectCall(dst: Temp, proc: Label) extends Instr {
    override def toString: String = (if (dst != null) s"$dst = " else "") + s"call $proc"
  }

  // Memory access
  case class Load(dst: Temp, base: Temp, offset: ConstTemp) extends Instr {
    override def toString: String =
      s"$dst = *" + (if (offset.value >= 0) s"($base + $offset)" else s"($base - ${ -offset.value })")
  }

  case class Store(src: Temp, base: Temp, offset: ConstTemp) extends Instr {
    override def toString: String =
      (if (offset.value >= 0) s"*($base + $offset)" else s"*($base - ${ -offset.value })") + s" = $src"
  }

  // Others
  case class Mark(lbl: Label) extends Instr {
    override def toString: String = s"$lbl:"
  }

  case class Memo(memo: String = "") extends Instr {
    override def toString: String = s"memo '$memo'"
  }

  type InstrSeq = List[Instr]

  implicit def __singletonInstr__(ins: Instr): InstrSeq = List(ins)
}
