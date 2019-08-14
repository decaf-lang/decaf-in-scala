package decaf.frontend.tac

class VTable(val name: String, val className: String, val parent: Option[VTable], val entries: List[Label]) {
  override def toString: String =
    s"VTABLE($name) {\n    ${ parent.map(_.name).getOrElse("<empty>") }\n    $className" +
      (entries.map { l => s"    $l;" } mkString "\n") + "\n}\n"
}