package decaf.frontend.tac

import decaf.frontend.tac.Tac.InstrSeq

class Proc(val label: Label, val paramMemo: Tac.Memo, val code: InstrSeq) {
  override def toString: String = s"FUNCTION($label) {\n$paramMemo\n" + (code.map {
    case i: Tac.Mark => i.toString
    case i => s"    $i"
  } mkString "\n") + "\n}\n"
}
