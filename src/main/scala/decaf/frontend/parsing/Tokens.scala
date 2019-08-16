package decaf.frontend.parsing

import decaf.frontend.tree.TreeNode

import scala.util.parsing.input.Positional

object Tokens {

  sealed trait Token extends Positional

  // Keywords are special tokens.

  sealed abstract class Keyword(val name: String) extends Token

  // Keywords that look like an identifier, and should be separated from identifiers.

  case object CLASS extends Keyword("class")

  case object EXTENDS extends Keyword("extends")

  case object STATIC extends Keyword("static")

  case object INT extends Keyword("int")

  case object BOOL extends Keyword("bool")

  case object STRING extends Keyword("string")

  case object VOID extends Keyword("void")

  case object NULL extends Keyword("null")

  case object TRUE extends Keyword("true")

  case object FALSE extends Keyword("false")

  case object IF extends Keyword("if")

  case object ELSE extends Keyword("else")

  case object WHILE extends Keyword("while")

  case object FOR extends Keyword("for")

  case object RETURN extends Keyword("return")

  case object BREAK extends Keyword("break")

  case object PRINT extends Keyword("Print")

  case object THIS extends Keyword("this")

  case object NEW extends Keyword("new")

  case object READ_INTEGER extends Keyword("ReadInteger")

  case object READ_LINE extends Keyword("ReadLine")

  case object INSTANCE_OF extends Keyword("instanceof")

  case object ASSIGN extends Keyword("=")

  // Keywords used as delimiters, typically a character.

  case object LPAREN extends Keyword("(")

  case object RPAREN extends Keyword(")")

  case object LBRACE extends Keyword("{")

  case object RBRACE extends Keyword("}")

  case object LBRACK extends Keyword("[")

  case object RBRACK extends Keyword("]")

  case object SEMI extends Keyword(";")

  case object COMMA extends Keyword(",")

  case object DOT extends Keyword(".")

  // Keywords used as expression operators.

  sealed abstract class Operator(val op: TreeNode.Op) extends Keyword(op.str)

  case object NEG extends Operator(TreeNode.NEG)

  case object NOT extends Operator(TreeNode.NOT)

  case object ADD extends Operator(TreeNode.ADD)

//  case object SUB extends Operator(TreeNode.SUB)

  case object MUL extends Operator(TreeNode.MUL)

  case object DIV extends Operator(TreeNode.DIV)

  case object MOD extends Operator(TreeNode.MOD)

  case object AND extends Operator(TreeNode.AND)

  case object OR extends Operator(TreeNode.OR)

  case object EQ extends Operator(TreeNode.EQ)

  case object NE extends Operator(TreeNode.NE)

  // NOTE: `<=` goes before `<`, since they have common prefix `<`.
  case object LE extends Operator(TreeNode.LE)

  case object LT extends Operator(TreeNode.LT)

  case object GE extends Operator(TreeNode.GE)

  case object GT extends Operator(TreeNode.GT)

  val KEYWORDS = List(
    CLASS, EXTENDS, STATIC, INT, BOOL, STRING, VOID, NULL, TRUE, FALSE, IF, ELSE, WHILE, FOR, RETURN, BREAK, PRINT,
    THIS, NEW, READ_INTEGER, READ_LINE, INSTANCE_OF, LPAREN, RPAREN, LBRACE, RBRACE, LBRACK, RBRACK, SEMI,
    COMMA, DOT, NEG, ADD, MUL, DIV, MOD, AND, OR, EQ, NE, LE, LT, GE, GT, ASSIGN, NOT
  )

  // Other tokens: literals and identifiers.

  case class INT_LIT(value: Int) extends Token

  case class STRING_LIT(value: String) extends Token

  case class IDENT(name: String) extends Token

}
