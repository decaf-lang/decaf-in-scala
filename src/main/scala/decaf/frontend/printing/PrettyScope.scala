package decaf.frontend.printing

import decaf.frontend.annot._

object PrettyScope {
  def pretty(scope: Scope)(implicit printer: IndentPrinter): Unit = scope match {
    case s: GlobalScope =>
      printer.writeln("GLOBAL SCOPE:")
      printer.withIndent {
        s.values.foreach { symbol => printer.writeln(symbol.toString) }
        s.values.foreach { symbol => pretty(symbol.scope) }
      }
    case s: ClassScope =>
      printer.writeln(s"CLASS SCOPE OF '${ s.owner.name }':")
      printer.withIndent {
        s.values.foreach { symbol => printer.writeln(symbol.toString) }
        s.values.foreach {
          case m: MethodSymbol => pretty(m.scope)
          case _ => // do not print
        }
      }
    case s: FormalScope =>
      printer.writeln(s"FORMAL SCOPE OF '${ s.owner.name }':")
      printer.withIndent {
        s.values.foreach { symbol => printer.writeln(symbol.toString) }
        pretty(s.nestedScope)
      }
    case s: LocalScope =>
      printer.writeln(s"LOCAL SCOPE:")
      printer.withIndent {
        s.values.foreach { symbol => printer.writeln(symbol.toString) }
        s.nestedScopes.foreach(pretty)
      }
  }
}
