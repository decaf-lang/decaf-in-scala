package decaf.parsing

import decaf.error.{ErrorIssuer, UnrecogCharError}
import decaf.parsing.antlr.DecafLexer
import org.antlr.v4.runtime.{CharStream, Token}

class Lexer(in: CharStream, issuer: ErrorIssuer) extends DecafLexer(in) {
  override def emit(): Token = getType match {
    case DecafLexer.UNRECOG_Char =>
      val token = super.emit()
      throw new UnrecogCharError(token.getText.head, Util.getPos(token))
    case _ => super.emit()
  }

  override def emitEOF(): Token = {
    if (_mode == DecafLexer.IN_STRING) {
      setType(DecafLexer.UNTERM_STRING)
      setText("UNTERM_STRING")
      val t = super.emit()
      popMode()
      t
    } else super.emitEOF()
  }
}
