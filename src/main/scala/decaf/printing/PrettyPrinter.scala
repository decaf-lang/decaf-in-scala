package decaf.printing

import decaf.lowlevel.log.IndentPrinter

/**
  * An indented pretty printer.
  *
  * @param printer the underlying indent printer
  * @tparam T type of printed element
  * @see [[IndentPrinter]]
  */
abstract class PrettyPrinter[T](printer: IndentPrinter) {

  /**
    * Pretty print the element.
    *
    * @param t element
    */
  def pretty(t: T): Unit

  /**
    * Flush output.
    */
  def flush(): Unit = {
    printer.flush()
  }

  /**
    * Indented printing. Will increase indent before entering `action` and decrese indent after leaving `action`.
    *
    * @param action print stuff to do, inside the indented section
    */
  def indent(action: => Unit): Unit = {
    printer.incIndent()
    action
    printer.decIndent()
  }
}
