package decaf.parsing

/** `Position` is the base trait for objects describing a position in a `document`.
  *
  * It provides functionality for:
  *   - generating a visual representation of this position (`longString`);
  *   - comparing two positions (`<`).
  *
  * To use this class for a concrete kind of `document`, implement the `lineContents` method.
  *
  * @author Martin Odersky
  * @author Adriaan Moors
  */
trait Position extends Ordered[Position] {

  /** The line number referred to by the position; line numbers start at 1. */
  def line: Int

  /** The column number referred to by the position; column numbers start at 1. */
  def column: Int

  /** The contents of the line at this position. (must not contain a new-line character).
    */
  protected def lineContents: String

  /** Returns a string representation of the `Position`, of the form `line.column`. */
  override def toString = s"($line,$column)"

  /** Returns a more ``visual'' representation of this position.
    * More precisely, the resulting string consists of two lines:
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

  /** Compare this position to another, by first comparing their line numbers,
    * and then -- if necessary -- using the columns to break a tie.
    *
    * @param `that` a `Position` to compare to this `Position`
    * @return true if this position's line number or (in case of equal line numbers)
    *         column is smaller than the corresponding components of `that`
    */
  override def compare(that: Position): Int =
    if (this.line == that.line && this.column == that.column) 0
    else if (this.line < that.line || this.line == that.line && this.column < that.column) -1
    else 1
}

/** Undefined position.
  *
  * @author Martin Odersky
  * @author Adriaan Moors
  */
object NoPosition extends Position {
  def line = 0

  def column = 0

  override def toString = "<undefined position>"

  override def longString = toString

  def lineContents = ""
}

trait Positional {

  /** The source position of this object, initially set to undefined. */
  var pos: Position = NoPosition

  /** If current source position is undefined, update it with given position `newpos`
    *
    * @return the object itself
    */
  def setPos(newpos: Position): this.type = {
    if (pos eq NoPosition) pos = newpos
    this
  }
}