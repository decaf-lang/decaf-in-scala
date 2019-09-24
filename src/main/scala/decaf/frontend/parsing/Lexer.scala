package decaf.frontend.parsing

import decaf.driver.error.UnrecogCharError
import decaf.frontend.parsing.antlr.DecafLexer
import org.antlr.v4.runtime.{CharStream, Token}

/**
  * Lexer. This class only contains a couple of hooks to handle lexer errors.
  * Antlr specification file: `src/main/antlr4/DecafLexer.g4`.
  *
  * @param in input stream
  */
class Lexer(in: CharStream) extends DecafLexer(in) {

  /**
    * Catch unrecognized character error (override the generated code).
    * This error will '''immediately interrupt''' the parsing.
    *
    * @return the emitted token if no error occurs
    */
  override def emit(): Token = getType match {
    case DecafLexer.UNRECOG_Char =>
      val token = super.emit()
      throw new UnrecogCharError(token.getText.head, Util.getPos(token))
    case _ => super.emit()
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
      val t = super.emit()
      popMode()
      t
    } else {
      super.emitEOF()
    }
  }
}
