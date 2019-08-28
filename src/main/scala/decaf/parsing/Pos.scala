package decaf.parsing

/**
  * A position, say a line number and a column number in a source file.
  *
  * Imported from scala.util.parsing.input.Position, and made a few adaptions.
  */
trait Pos extends Ordered[Pos] {

  /** The line number referred to by the position; line numbers start at 1. */
  def line: Int

  /** The column number referred to by the position; column numbers start at 1. */
  def column: Int

  /** The contents of the line at this position. (must not contain a new-line character). */
  protected def lineContents: String

  /** Returns a string representation. */
  override def toString = s"($line,$column)"

  /**
    * Returns a more "visual" representation of this position. More precisely, the resulting string consists of
    * two lines:
    *   1. the line in the document referred to by this position
    *   2. a caret indicating the column
    *
    * Example:
    * {{{
    *    List(this, is, a, line, from, the, document)
    *                 ^
    * }}}
    */
  def longString = lineContents + "\n" + lineContents.take(column - 1).map { x => if (x == '\t') x else ' ' } + "^"

  /** Compare this position to another, by first comparing their line numbers, and then -- if necessary -- using the
    * columns to break a tie.
    *
    * @param that a position to compare to this position
    * @return true if this position's line number or (in case of equal line numbers) column is smaller than the
    *         corresponding components of that
    */
  override def compare(that: Pos): Int =
    if (this.line == that.line && this.column == that.column) 0
    else if (this.line < that.line || this.line == that.line && this.column < that.column) -1
    else 1
}

/**
  * Undefined position.
  */
object NoPos extends Pos {
  def line = 0

  def column = 0

  override def toString = "<undefined position>"

  override def longString = toString

  def lineContents = ""
}

/**
  * Something that has a position.
  */
trait Positional {
  var pos: Pos = NoPos

  def setPos(pos: Pos): this.type = {
    this.pos = pos
    this
  }
}
