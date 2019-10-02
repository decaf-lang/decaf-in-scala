package decaf.frontend.parsing

import decaf.driver.error._
import decaf.frontend.parsing.Util.getPos
import decaf.frontend.parsing.antlr.DecafLexer
import org.antlr.v4.runtime.{CharStream, Token}

/**
  * Lexer. This class only contains a couple of hooks to handle lexer errors.
  * Antlr specification file: `src/main/antlr4/DecafLexer.g4`.
  *
  * @param in input stream
  */
class Lexer(in: CharStream, errorIssuer: ErrorIssuer) extends DecafLexer(in) {

  // Temporary variables for parsing a string literal
  private val buffer: StringBuilder = new StringBuilder
  private var startPos: Pos = NoPos

  /**
    * Lexer error handler and string literal parser.
    * This method will be invoked before a token is returned to the parser.
    *
    * A special case is that unrecognized character error will '''immediately interrupt''' the parsing.
    * Other lexer errors can be recovered.
    *
    * @return the emitted token
    */
  override def emit: Token = getType match {
    // unrecognized char: immediately interrupt
    case DecafLexer.UNRECOG_Char =>
      val token = super.emit
      throw new UnrecogCharError(token.getText.head, getPos(token))
    // integer too large
    case DecafLexer.INT_LIT =>
      val token = super.emit
      var literal = "0"
      try {
        literal = token.getText.toInt.toString
      } catch {
        case _: NumberFormatException => // not a valid 32-bit integer
          errorIssuer.issue(new IntTooLargeError(token.getText, getPos(token)))
      }
      setText(literal)
      super.emit
    // string literal
    case DecafLexer.UNTERM_STRING =>
      errorIssuer.issue(new UntermStrError(buffer.toString, startPos))
      super.emit
    case DecafLexer.OPEN_STRING =>
      buffer.clear()
      buffer += '"'
      val token = super.emit
      startPos = getPos(token)
      token
    case DecafLexer.ERROR_NEWLINE =>
      val token = super.emit
      errorIssuer.issue(new NewlineInStrError(buffer.toString, getPos(token)))
      token
    case DecafLexer.BAD_ESC =>
      val token = super.emit
      errorIssuer.issue(new BadEscCharError(getPos(token)))
      token
    case DecafLexer.ESC | DecafLexer.VALID_CHAR =>
      val token = super.emit
      buffer ++= token.getText
      token
    case DecafLexer.CLOSE_STRING =>
      buffer += '"'
      setText(buffer.toString)
      super.emit
    case _ => super.emit
  }

  /**
    * Enforce the emission of `UNTERM_STRING` token when an unterminated string is encountered (override the generated
    * code).
    *
    * Motivation: Antlr cannot capture the special EOF token if we simply write something like
    * {{{ UNTERM_STRING: EOF -> popMode; }}}
    * Thus, let's handle it ourselves! When an EOF is seen, we check if we are parsing a string literal (mode is
    * `IN_STRING`). If so, we modifies this token as type `UNTERM_STRING` and switch to the default mode (by popping
    * the IN_STRING` mode).
    *
    * @return the emitted token
    **/
  override def emitEOF(): Token = {
    if (_mode == DecafLexer.IN_STRING) {
      setType(DecafLexer.UNTERM_STRING)
      setText("UNTERM_STRING")
      val t = emit
      popMode()
      t
    } else {
      super.emitEOF()
    }
  }
}
