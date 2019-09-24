package decaf.frontend.parsing

/**
  * A position, say a line number and a column number in a source file.
  * Imported from [[scala.util.parsing.input.Position]], and made a few adaptions & simplifications.
  *
  * @param line   line number referred to by the position (start at 1)
  * @param column column number referred to by the position (start at 1)
  */
class Pos(val line: Int, val column: Int) extends Ordered[Pos] {

  /** Returns a string representation. */
  override def toString = s"($line,$column)"

  /** Compare two positions.
    * First comparing their line numbers, and then -- if necessary -- using the columns to break a tie.
    *
    * @param that a position to compare to
    * @return true if this position's line number or (in case of equal line numbers) column is smaller than the
    *         corresponding components of that
    */
  override def compare(that: Pos): Int =
    if (this.line == that.line && this.column == that.column) {
      0
    } else if (this.line < that.line || this.line == that.line && this.column < that.column) {
      -1
    } else {
      1
    }
}

/**
  * Undefined position.
  */
object NoPos extends Pos(0, 0) {

  override def toString = "<undefined position>"
}

/**
  * Something that has a position.
  */
trait Positional {

  var pos: Pos = NoPos

  /**
    * Set position.
    *
    * @param pos new position
    * @return the updated object
    */
  def setPos(pos: Pos): this.type = {
    this.pos = pos
    this
  }
}
