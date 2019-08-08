package decaf.frontend.tree

import scala.util.parsing.input.Positional

trait Node extends Product with Positional

object TreeNode {

  /**
    * Identifier.
    *
    * @param name its name
    */
  case class Id(name: String) extends Node


  /**
    * Any definition must be named.
    */
  trait Def extends Node {
    val id: Id
    val name: String = id.name
  }

  /**
    * A general form of variable declaration.
    */
  trait VarDecl extends Def {
    type TypeLitT <: Node

    val typeLit: TypeLitT
  }

  /**
    * Operator.
    */
  trait Op

  /**
    * Unary operator.
    */
  trait UnaryOp extends Op

  /**
    * Integer negation: {{{ - }}}.
    */
  case object NEG extends UnaryOp

  /**
    * Logical negation: {{{ ! }}}.
    */
  case object NOT extends UnaryOp

  trait BinaryOp extends Op

  trait ArithOp extends BinaryOp

  /**
    * Addition: {{{ + }}}.
    */
  case object ADD extends ArithOp

  /**
    * Subtraction: {{{ - }}}.
    */
  case object SUB extends ArithOp

  /**
    * Multiplication: {{{ * }}}.
    */
  case object MUL extends ArithOp

  /**
    * Division: {{{ / }}}.
    */
  case object DIV extends ArithOp

  /**
    * Modulo: {{{ % }}}.
    */
  case object MOD extends ArithOp

  trait LogicOp extends BinaryOp

  /**
    * Logical and: {{{ && }}}.
    */
  case object AND extends LogicOp

  /**
    * Logical or: {{{ || }}}.
    */
  case object OR extends LogicOp

  trait EqOp extends BinaryOp

  /**
    * Equal to: {{{ == }}}.
    */
  case object EQ extends EqOp

  /**
    * Not equal to: {{{ != }}}.
    */
  case object NE extends EqOp

  trait CmpOp extends BinaryOp

  /**
    * Less than: {{{ < }}}.
    */
  case object LT extends CmpOp

  /**
    * Less than or equal to: {{{ <= }}}.
    */
  case object LE extends CmpOp

  /**
    * Greater than: {{{ > }}}.
    */
  case object GT extends CmpOp

  /**
    * Greater than or equal to: {{{ >= }}}.
    */
  case object GE extends CmpOp

}