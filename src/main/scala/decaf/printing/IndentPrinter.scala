package decaf.printing

class IndentPrinter(val spaces: Int = 4) {
  private val sb: StringBuilder = new StringBuilder

  private var level: Int = 0

  def indent(): Unit = level += 1

  def dedent(): Unit = if (level > 0) level -= 1

  def withIndent(action: => Unit): Unit = {
    indent()
    action
    dedent()
  }

  def writeln(string: String = ""): Unit = {
    sb ++= " " * level * spaces
    sb ++= string
    sb += '\n'
  }

  def write(string: String): Unit = ???

  def andThen(action: => Unit): Unit = {
    action
    writeln()
  }

  override def toString: String = sb.toString
}
