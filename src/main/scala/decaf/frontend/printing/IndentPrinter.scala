package decaf.frontend.printing

class IndentPrinter(val spaces: Int = 4) {
  private val sb: StringBuilder = new StringBuilder

  private var level: Int = 0

  def indent(): Unit = level += 1

  def dedent(): Unit = if (level > 0) level -= 1

  def writeln(string: String): Unit = {
    sb ++= " " * level * spaces
    sb ++= string
    sb += '\n'
  }

  override def toString: String = sb.toString
}
