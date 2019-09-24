package decaf.frontend.parsing

import decaf.frontend.tree.SyntaxTree.{Block, Stmt}
import org.antlr.v4.runtime.{ParserRuleContext, Token}

object Util {

  /**
    * Set the position of a node `e` as the starting position of the first token.
    *
    * @param ctx parsed rule context
    * @param e   node
    * @return updated node
    */
  def positioned[T <: Positional](ctx: ParserRuleContext)(e: T): T = e.setPos(
    new Pos(ctx.getStart.getLine, ctx.getStart.getCharPositionInLine + 1))

  /**
    * Obtain the starting position of a `token`.
    *
    * @param token token
    * @return starting position
    */
  def getPos(token: Token): Pos = new Pos(token.getLine, token.getCharPositionInLine + 1)

  /**
    * Wrap a statement as a block.
    *
    * @param stmt statement
    * @return `stmt` itself if it is already a block, or a new block having `stmt` as the sole object
    */
  implicit def blocked(stmt: Stmt): Block = stmt match {
    case b: Block => b
    case _ => Block(List(stmt)).setPos(stmt.pos)
  }
}
