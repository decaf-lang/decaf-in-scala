package decaf.printing

import decaf.lowlevel.log.IndentPrinter

abstract class PrettyPrinter[T](printer: IndentPrinter) {

  def pretty(t: T): Unit

  def flush(): Unit = {
    printer.flush()
  }

  def indent(action: => Unit): Unit = {
    printer.incIndent()
    action
    printer.decIndent()
  }
}
