package decaf.printing

/**
  * A simple indent printer. You can increase indentation level by wrapping your actions in `withIndent`.
  * For example, it can print:
  * {{{
  *    node1
  *         node2
  *         node3
  *             leaf1
  *         leaf2
  *    leaf3
  * }}}
  *
  * @param spaces how many spaces are for one indentation level?
  */
class IndentPrinter(val spaces: Int = 4) {
  private val sb: StringBuilder = new StringBuilder

  private var level: Int = 0

  /**
    * Increase the indentation level by one, do the printing stuff in `action`, and then recover the indentation level.
    *
    * @param action the printing actions which need be indented
    */
  def withIndent(action: => Unit): Unit = {
    level += 1
    action
    level -= 1
  }

  /**
    * Print a string and a end of line ('\n').
    *
    * @param string the string
    */
  def writeln(string: String = ""): Unit = {
    sb ++= " " * level * spaces
    sb ++= string
    sb += '\n'
  }

  /**
    * Get the indented string.
    */
  override def toString: String = sb.toString
}
