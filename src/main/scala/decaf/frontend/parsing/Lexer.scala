package decaf.frontend.parsing

import scala.util.parsing.combinator.RegexParsers

class Lexer extends RegexParsers {
  def decimal: Parser[Int] =
    """[0-9]+""".r ^^ {
      _.toInt
    }

  def hex: Parser[Int] =
    """0[Xx][0-9A-Fa-f]+""".r ^^ {
      _.toInt
    }

  def boolean: Parser[Boolean] = "true" ^^^ true | "false" ^^^ false

  def quotedChar: Parser[Char] = "\\\"" ^^^ '"' | "\\\\" ^^^ '\\' | "\\t" ^^^ '\t' | "\\n" ^^^ '\n' |
    """[^"]""".r ^^ {
      _.head
    }

  def quotedString: Parser[String] = '"' ~> quotedChar.* <~ '"' ^^ {
    _.mkString
  }

  def identifier: Parser[String] = """[A-Za-z][_0-9A-Za-z]*""".r

  override protected val whiteSpace = """(\s|//.*)+""".r
}
