package decaf.frontend.parsing

import decaf.frontend.parsing.Tokens._

import scala.util.parsing.combinator.RegexParsers

class Lexer extends RegexParsers {
  override protected val whiteSpace = """(\s|//.*)+""".r

  def keyword: Parser[Keyword] = KEYWORDS.map { k => positioned(k.name ^^^ k) }.reduce(_ | _)

  def intLit: Parser[INT_LIT] = positioned { (decimal | hex) ^^ INT_LIT }

  def stringLit: Parser[STRING_LIT] = positioned { quotedString ^^ STRING_LIT }

  def ident: Parser[IDENT] = positioned { identifier ^^ IDENT }

  def tokens: Parser[Seq[Token]] = phrase(rep(intLit | stringLit | ident ^^ {
    t =>
      KEYWORDS.find(_.name == t.name) match {
        case Some(tk) => tk
        case None => t
      }
  } | keyword))

  //  def tokens: Parser[Seq[Token]] = phrase(rep(intLit | stringLit | keyword | ident))

  private def decimal: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

  private def hex: Parser[Int] = """0[Xx][0-9A-Fa-f]+""".r ^^ { _.toInt }

  private def quotedString: Parser[String] = (in: Input) => {
    val sb = new StringBuilder

    @scala.annotation.tailrec
    def continue(r: Input): ParseResult[String] = {
      if (r.atEnd) Failure("quoted string not closed", r)
      else r.first match {
        case '"' =>
          Success(sb.toString, r.rest)
        case '\\' =>
          if (r.rest.atEnd) Failure("quoted char not complete", r)
          else r.rest.first match {
            case '"' => sb += '"'; continue(r.rest.rest)
            case '\\' => sb += '\\'; continue(r.rest.rest)
            case 't' => sb += '\t'; continue(r.rest.rest)
            case 'n' => sb += '\n'; continue(r.rest.rest)
            case other => Failure(s"invalid quoted char: \\$other", r)
          }
        case c => sb += c; continue(r.rest)
      }
    }

    if (in.atEnd) Failure("end of file", in)
    else if (in.first == '"') continue(in.rest)
    else Failure("quoted string must be started with a '\"'", in)
  }

  def identifier: Parser[String] = """[A-Za-z][_0-9A-Za-z]*""".r
}
