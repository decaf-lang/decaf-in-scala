package decaf.printing

import decaf.frontend.annot._
import decaf.lowlevel.log.IndentPrinter

/** Pretty print a scope. Output of PA2. */
class PrettyScope(printer: IndentPrinter) extends PrettyPrinter[Scope](printer) {

  override def pretty(scope: Scope): Unit = scope match {
    case s: GlobalScope =>
      printer.println("GLOBAL SCOPE:")
      indent {
        if (s.isEmpty) {
          printer.println("<empty>")
        } else {
          s.values.foreach { symbol => printer.println(symbol.toString) }
        }
        s.values.foreach { symbol => pretty(symbol.scope) }
      }
    case s: ClassScope =>
      printer.println(s"CLASS SCOPE OF '${ s.owner.name }':")
      indent {
        if (s.isEmpty) {
          printer.println("<empty>")
        } else {
          s.values.foreach { symbol => printer.println(symbol.toString) }
        }
        s.values.foreach {
          case m: MethodSymbol => pretty(m.scope)
          case _ => // do not print
        }
      }
    case s: FormalScope =>
      printer.println(s"FORMAL SCOPE OF '${ s.owner.name }':")
      indent {
        if (s.isEmpty) {
          printer.println("<empty>")
        } else {
          s.values.foreach { symbol => printer.println(symbol.toString) }
        }
        pretty(s.nestedScope)
      }
    case s: LocalScope =>
      printer.println(s"LOCAL SCOPE:")
      indent {
        if (s.isEmpty) {
          printer.println("<empty>")
        } else {
          s.values.foreach { symbol => printer.println(symbol.toString) }
        }
        s.nestedScopes.foreach(pretty)
      }
  }
}
