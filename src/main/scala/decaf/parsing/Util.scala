package decaf.parsing

import decaf.tree.SyntaxTree.{Stmt, Block}
import org.antlr.v4.runtime.{ParserRuleContext, Token}

object Util {
  /**
    * Quote a string literal. Must call this when print out a string literal. Otherwise the special control characters
    * (like \n) will ruin the format.
    *
    * @param str a string
    * @return the quoted form
    */
  def quote(str: String): String = {
    val sb = new StringBuilder
    str.foreach {
      case '"' => sb ++= "\\\""
      case '\n' => sb ++= "\\n"
      case '\r' => sb ++= "\\r"
      case '\t' => sb ++= "\\t"
      case '\\' => sb ++= "\\\\"
      case c => sb += c
    }
    "\"" + sb.toString + "\""
  }

  /**
    * Set the position of a node `e` as the starting position of the first token.
    *
    * @param ctx the parsed rule context
    * @param e   the node
    * @return the node after position is set
    */
  def positioned[T <: Positional](ctx: ParserRuleContext)(e: T): T = e.setPos(new Pos {
    override def line: Int = ctx.getStart.getLine

    override def column: Int = ctx.getStart.getCharPositionInLine + 1

    override protected def lineContents: String = ???
  })

  /**
    * Obtain the starting position of the `token`.
    *
    * @param token the token
    * @return the starting position
    */
  def getPos(token: Token): Pos = new Pos {
    override def line: Int = token.getLine

    override def column: Int = token.getCharPositionInLine + 1

    override protected def lineContents: String = ???
  }

  implicit def blocked(stmt: Stmt): Block = stmt match {
    case b: Block => b
    case _ => Block(List(stmt)).setPos(stmt.pos)
  }
}
