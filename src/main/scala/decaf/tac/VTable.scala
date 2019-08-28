package decaf.tac

import decaf.annot.MemberVarSymbol

class VTable(val name: String, val className: String, val parent: Option[VTable],
             val memberMethods: List[Label], val memberVars: List[MemberVarSymbol]) {
  override def toString: String =
    s"VTABLE($name) {\n    ${ parent.map(_.name).getOrElse("<empty>") }\n    $className" +
      (memberMethods.map { l => s"\n    $l;" }.mkString) + "\n}\n"
}