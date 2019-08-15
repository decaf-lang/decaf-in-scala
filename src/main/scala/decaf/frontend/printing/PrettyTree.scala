package decaf.frontend.printing

import decaf.frontend.annot.Annotated
import decaf.frontend.parsing.StringUtil
import decaf.frontend.tree.TreeNode.{Id, Node}

object PrettyTree {

  def prettyElement(element: Any)(implicit printer: IndentPrinter, showAnnot: Boolean = false): Unit = element match {
    case e: Node with Annotated[_] => pretty(e)
    case Id(name) => printer.writeln(name)
    case Some(e) => prettyElement(e)
    case None => printer.writeln("<none>")
    case es: List[_] =>
      printer.writeln("list")
      printer.indent()
      if (es.isEmpty) printer.writeln("<empty>") else es.foreach(prettyElement)
      printer.dedent()
    case e: String => printer.writeln(StringUtil.quote(e))
    case e => printer.writeln(e.toString)
  }

  def pretty(node: Node with Annotated[_])(implicit printer: IndentPrinter, showAnnot: Boolean = false): Unit = {
    printer.writeln(node.productPrefix + (if (showAnnot) s" { ${ node.annot } }" else ""))
    printer.indent()
    node.productIterator.foreach(prettyElement)
    printer.dedent()
  }
}
