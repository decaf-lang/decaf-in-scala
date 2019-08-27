package decaf.parsing

import org.antlr.v4.runtime.Token

object Util {
  def getPos(token: Token): Pos = new Pos {
    override def line: Int = token.getLine

    override def column: Int = token.getCharPositionInLine + 1

    override protected def lineContents: String = ???
  }
}
