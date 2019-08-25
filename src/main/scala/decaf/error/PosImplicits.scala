package decaf.error

import scala.util.parsing.input.Position

object PosImplicits {

  implicit object _PosOrd extends Ordering[Position] {
    override def compare(x: Position, y: Position): Int =
      if (x.line == y.line && x.column == y.column) 0
      else if (x < y) -1
      else 1
  }

}
