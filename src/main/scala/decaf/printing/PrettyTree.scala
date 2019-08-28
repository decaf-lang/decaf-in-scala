package decaf.printing

import decaf.annot.Annotated
import decaf.parsing.StringUtil
import decaf.tree.TreeNode.{Id, Node}

object PrettyTree {

  case class PrettyConfig(showPos: Boolean = false, showAnnot: Boolean = false)

  def prettyElement(element: Any)(implicit printer: IndentPrinter, config: PrettyConfig): Unit = element match {
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

  def pretty(node: Node with Annotated[_])(implicit printer: IndentPrinter, config: PrettyConfig): Unit = {
    val annotStr = if (config.showAnnot) s" { ${ node.annot } }" else ""
    val posStr = if (config.showPos) s" @ (${ node.pos.line },${ node.pos.column })"

    printer.writeln(node.productPrefix + annotStr + posStr)
    printer.indent()
    node.productIterator.foreach(prettyElement)
    printer.dedent()
  }
}
