package decaf.tree

import decaf.parsing.Positional

object TreeNode {

  /**
    * Every tree node is a product, so that we can easily access all its members/children.
    * Also, every tree node has a position (say the position in the source file).
    */
  trait Node extends Product with Positional

  /**
    * Identifier.
    *
    * @param name its name
    */
  case class Id(name: String) extends Node {
    override def toString: String = name
  }

  implicit def getNameOfId(id: Id): String = id.name

  /**
    * Any definition must be named.
    */
  trait Def extends Node {
    def id: Id

    def name: String = id.name
  }

  /**
    * A general form of variable declaration: it consists of a type literal indicating its type, and an identifier.
    */
  trait Var extends Def {
    type TypeLitT <: Node

    val typeLit: TypeLitT
  }

  /**
    * Operator.
    */
  trait Op {
    val str: String

    override def toString: String = str
  }

  implicit def getStrOfOp(op: Op): String = op.str

  /**
    * Unary operator.
    */
  trait UnaryOp extends Op

  /**
    * Integer negation: {{{ - }}}.
    */
  case object NEG extends UnaryOp {
    override val str: String = "-"
  }

  /**
    * Logical negation: {{{ ! }}}.
    */
  case object NOT extends UnaryOp {
    override val str: String = "!"
  }

  trait BinaryOp extends Op

  trait ArithOp extends BinaryOp

  /**
    * Addition: {{{ + }}}.
    */
  case object ADD extends ArithOp {
    override val str: String = "+"
  }

  /**
    * Subtraction: {{{ - }}}.
    */
  case object SUB extends ArithOp {
    override val str: String = "-"
  }

  /**
    * Multiplication: {{{ * }}}.
    */
  case object MUL extends ArithOp {
    override val str: String = "*"
  }

  /**
    * Division: {{{ / }}}.
    */
  case object DIV extends ArithOp {
    override val str: String = "/"
  }

  /**
    * Modulo: {{{ % }}}.
    */
  case object MOD extends ArithOp {
    override val str: String = "%"
  }

  trait LogicOp extends BinaryOp

  /**
    * Logical and: {{{ && }}}.
    */
  case object AND extends LogicOp {
    override val str: String = "&&"
  }

  /**
    * Logical or: {{{ || }}}.
    */
  case object OR extends LogicOp {
    override val str: String = "||"
  }

  trait EqOrCmpOp extends BinaryOp

  trait EqOp extends EqOrCmpOp

  /**
    * Equal to: {{{ == }}}.
    */
  case object EQ extends EqOp {
    override val str: String = "=="
  }

  /**
    * Not equal to: {{{ != }}}.
    */
  case object NE extends EqOp {
    override val str: String = "!="
  }

  trait CmpOp extends EqOrCmpOp

  /**
    * Less than: {{{ < }}}.
    */
  case object LT extends CmpOp {
    override val str: String = "<"
  }

  /**
    * Less than or equal to: {{{ <= }}}.
    */
  case object LE extends CmpOp {
    override val str: String = "<="
  }

  /**
    * Greater than: {{{ > }}}.
    */
  case object GT extends CmpOp {
    override val str: String = ">"
  }

  /**
    * Greater than or equal to: {{{ >= }}}.
    */
  case object GE extends CmpOp {
    override val str: String = ">="
  }

}