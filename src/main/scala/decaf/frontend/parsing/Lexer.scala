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

  private def quoted: Parser[String] = (in: Input) => {
    val sb = new StringBuilder

    def scan(r: Input): ParseResult[String] = {
      if (r.atEnd) Failure("string literal not closed", r)
      else r.first match {
        case '"' => Success(sb.toString, r.rest)
        case '\\' => scanQuoted(r.rest)
        case c => sb += c; scan(r.rest)
      }
    }

    def scanQuoted(r: Input): ParseResult[String] = {
      if (r.atEnd) Failure("string literal not closed", r)
      else r.first match {
        case '"' => sb += '"'; scan(r.rest)
        case '\\' => sb += '\\'; scan(r.rest)
        case 't' => sb += '\t'; scan(r.rest)
        case 'n' => sb += '\n'; scan(r.rest)
        case other => Failure(s"invalid quoted char: \\$other", r)
      }
    }

    scan(in)
  }

  def quotedString: Parser[String] = '"' ~> quoted

  def identifier: Parser[String] = """[A-Za-z][_0-9A-Za-z]*""".r

  override protected val whiteSpace = """(\s|//.*)+""".r
}
