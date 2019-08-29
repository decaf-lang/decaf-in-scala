package decaf.printing

import decaf.parsing.Util.quote
import decaf.tac.Tac._
import decaf.tac.{Proc, VTable}

/**
  * PA3 TAC script output.
  */
object PrettyTac {
  def pretty(program: Program)(implicit printer: IndentPrinter): Unit = {
    program.vtbls.foreach { t => prettyVTable(t); printer.writeln() }
    program.procs.foreach { p => prettyProc(p); printer.writeln() }
  }

  def prettyVTable(vtbl: VTable)(implicit printer: IndentPrinter): Unit = {
    printer.writeln(s"VTABLE(${ vtbl.name }) {")
    printer.withIndent {
      printer.writeln(vtbl.parent.map(_.name).getOrElse("<empty>"))
      printer.writeln(vtbl.className)
      vtbl.memberMethods.foreach { lbl => printer.writeln(lbl + ";") }
    }
    printer.writeln("}")
  }

  def prettyProc(proc: Proc)(implicit printer: IndentPrinter): Unit = {
    printer.writeln(s"FUNCTION(${ proc.label }) {")
    printer.writeln(prettyInstrOf(proc.paramMemo))
    proc.code.foreach { // TODO split by mark and indent print each block
      case mark: Mark => printer.writeln(prettyInstrOf(mark))
      case instr => printer.withIndent { printer.writeln(prettyInstrOf(instr)) }
    }
    printer.writeln("}")
  }

  def prettyInstrOf(instr: Instr): String = instr match {
    case Move(dst, src) => s"$dst = $src"
    case LoadVTbl(dst, vtbl) => s"$dst = VTBL<${ vtbl.name }>"
    case LoadImm4(dst, int) => s"$dst = $int"
    case LoadStrConst(dst, str) => s"$dst = ${ quote(str) }"
    case Add(dst, src1, src2) => s"$dst = ($src1 + $src2)"
    case Sub(dst, src1, src2) => s"$dst = ($src1 - $src2)"
    case Mul(dst, src1, src2) => s"$dst = ($src1 * $src2)"
    case Div(dst, src1, src2) => s"$dst = ($src1 / $src2)"
    case Mod(dst, src1, src2) => s"$dst = ($src1 % $src2)"
    case Neg(dst, src) => s"$dst = - $src"
    case Equ(dst, src1, src2) => s"$dst = ($src1 == $src2)"
    case Neq(dst, src1, src2) => s"$dst = ($src1 != $src2)"
    case Les(dst, src1, src2) => s"$dst = ($src1 < $src2)"
    case Leq(dst, src1, src2) => s"$dst = ($src1 <= $src2)"
    case Gtr(dst, src1, src2) => s"$dst = ($src1 > $src2)"
    case Geq(dst, src1, src2) => s"$dst = ($src1 >= $src2)"
    case LAnd(dst, src1, src2) => s"$dst = ($src1 && $src2)"
    case LOr(dst, src1, src2) => s"$dst = ($src1 || $src2)"
    case LNot(dst, src) => s"$dst = ! $src"
    case Branch(lbl) => s"branch $lbl"
    case BEqZ(cond, lbl) => s"if ($cond == 0) branch $lbl"
    case BNeZ(cond, lbl) => s"if ($cond != 0) branch $lbl"
    case Ret(src) => "return " + (if (src == null) "<empty>" else src.toString)
    case Parm(src) => s"parm $src"
    case IndirectCall(dst, proc) => (if (dst != null) s"$dst = " else "") + s"call $proc"
    case DirectCall(dst, proc) => (if (dst != null) s"$dst = " else "") + s"call $proc"
    case Load(dst, base, offset) =>
      s"$dst = *" + (if (offset.value >= 0) s"($base + $offset)" else s"($base - ${ -offset.value })")
    case Store(src, base, offset) =>
      (if (offset.value >= 0) s"*($base + $offset)" else s"*($base - ${ -offset.value })") + s" = $src"
    case Mark(lbl) => s"$lbl:"
    case Memo(memo) => s"memo '$memo'"
  }
}
