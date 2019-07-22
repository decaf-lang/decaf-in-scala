package decaf.frontend.core

import scala.util.parsing.input.Positional

object Trees {

  abstract class Tree extends Positional

  /**
    * A top-level decaf program, consisting of many class definitions.
    */
  case class TopLevel(classes: List[ClassDef]) extends Tree

  /**
    * Class definition:
    * {{{
    *   class <id> [extends <parent>] { <fields> }
    * }}}
    *
    * @param id     class identifier
    * @param parent parent class identifier (optional)
    * @param fields fields/members
    */
  case class ClassDef(id: Id, parent: Option[Id], fields: List[Field]) extends Tree

  /**
    * Field/Member of a class.
    */
  abstract class Field extends Tree

  /**
    * Member variable declaration:
    * {{{
    *  <typ> <id>;
    * }}}
    * Initialization is not supported.
    *
    * @param typ type
    * @param id  identifier
    */
  case class VarDef(typ: Type, id: Id) extends Field

  /**
    * Member method definition:
    * {{{
    *   [static] <returnType> <id> (<typ1> <id1>, <typ2> <id2>, ...) { <body> }
    * }}}
    * Decaf has static methods but _no_ static variables, strange!
    *
    * @param isStatic is this method static?
    * @param sig      method signature
    * @param body     method body (a statement block)
    */
  case class MethodDef(isStatic: Boolean, sig: Sig, body: Block) extends Field

  /**
    * Method signature.
    *
    * @param returnType return type
    * @param id         method identifier
    * @param params     parameters, each is a typed identifier (or _formal_, as said in the language specification)
    */
  case class Sig(returnType: Type, id: Id, params: List[VarDef]) extends Tree

  /**
    * Type. Decaf only supports
    * - basic types (integer, boolean, string, void),
    * - class types (using class identifiers), and
    * - array types (whose element could be any type, but homogeneous).
    */
  abstract class Type extends Tree

  /**
    * 32 bit integer type: {{{ int }}}
    *
    * NOTE: why not `object TInt`?
    * The type `int` can appear multiple times at different positions, and hence it really _cannot_ be regarded as
    * a singleton object.
    */
  case class TInt() extends Type

  /**
    * Boolean type: {{{ bool }}}
    */
  case class TBool() extends Type

  /**
    * String type: {{{ string }}}
    */
  case class TString() extends Type

  /**
    * Void type: {{{ void }}}
    * For method return type _only_.
    */
  case class TVoid() extends Type

  /**
    * Class type:
    * {{{
    *   class <id>
    * }}}
    *
    * @param id class identifier
    */
  case class TClass(id: Id) extends Type

  /**
    * Array type:
    * {{{
    *   <elemType>[]
    * }}}
    *
    * @param elemType element type
    */
  case class TArray(elemType: Type) extends Type

  /**
    * Statement.
    */
  abstract class Stmt extends Tree

  /**
    * Local variable declaration:
    * {{{
    *  <typ> <id>;
    * }}}
    * Initialization is not supported.
    *
    * @param typ type
    * @param id  identifier
    */
  case class LocalVarDef(typ: Type, id: Id) extends Stmt

  /**
    * Simple statement.
    */
  abstract class SimpleStmt extends Stmt

  /**
    * Assignment:
    * {{{
    *   <lhs> = <rhs>;
    * }}}
    *
    * @param lhs left hand side, i.e. the left-value to be assigned
    * @param rhs right hand side, i.e. the value to assign
    */
  case class Assign(lhs: Expr, rhs: Expr) extends SimpleStmt

  /**
    * Expression evaluation, typically a method call.
    *
    * @param expr expression to be evaluated
    */
  case class ExprEval(expr: Expr) extends SimpleStmt

  /**
    * Empty statement, do nothing.
    */
  case class Skip() extends SimpleStmt

  /**
    * If statement:
    * {{{
    *   if (<cond>) <trueBranch> [else <falseBranch>]
    * }}}
    *
    * @param cond        condition
    * @param trueBranch  body to execute if condition holds
    * @param falseBranch body to execute if condition does NOT hold
    */
  case class If(cond: Expr, trueBranch: Stmt, falseBranch: Option[Stmt]) extends Stmt

  /**
    * While statement:
    * {{{
    *   while (<cond>) <body>
    * }}}
    *
    * @param cond condition
    * @param body loop body to execute if condition holds
    */
  case class While(cond: Expr, body: Stmt) extends Stmt

  /**
    * For statement:
    * {{{
    *   for (<init>; <cond>; <update>) <body>
    * }}}
    *
    * @param init   initialization before entering the loop (simple statements _only_)
    * @param cond   condition
    * @param update updating actions after every loop iteration (simple statements _only_)
    * @param body   body to execute if condition holds
    */
  case class For(init: SimpleStmt, cond: Expr, update: SimpleStmt, body: Stmt) extends Stmt

  /**
    * Break statement:
    * {{{
    *   break;
    * }}}
    *
    * Jump out of the _innermost_ loop.
    */
  case class Break() extends Stmt

  /**
    * Method return statement:
    * {{{
    *   return [<expr>];
    * }}}
    *
    * @param expr value to be returned, or nothing if not given (in this case, the method return type must be void)
    */
  case class Return(expr: Option[Expr]) extends Stmt

  /**
    * Print statement:
    * {{{
    *   Print(<expr1>, <expr2>, ...);
    * }}}
    *
    * @param exprs values to be printed
    */
  case class Print(exprs: List[Expr]) extends Stmt

  /**
    * Statement block:
    * {{{
    *   { <stmt1> <stmt2> ... }
    * }}}
    *
    * @param stmts statements inside the block
    */
  case class Block(stmts: List[Stmt]) extends Stmt

  /**
    * Expression.
    */
  abstract class Expr extends Tree

  /**
    * Literal/Constant.
    */
  abstract class Lit extends Expr {
    type T
    val value: T
  }

  /**
    * Integer literal.
    */
  case class IntLit(override val value: Int) extends Lit {
    type T = Int
  }

  /**
    * Boolean literal.
    */
  case class BoolLit(override val value: Boolean) extends Lit {
    type T = Boolean
  }

  /**
    * String literal.
    */
  case class StringLit(override val value: String) extends Lit {
    type T = String
  }

  /**
    * Null literal: {{{ null }}}
    */
  case class NullLit() extends Lit {
    type T = Null

    val value = null
  }

  /**
    * Field selection:
    * {{{
    *   <receiver>.<field>
    * }}}
    *
    * @param receiver target instance
    * @param field    identifier of the selected field
    */
  case class FieldSel(receiver: Expr, field: Id) extends Expr

  /**
    * Array element selection by index:
    * {{{
    *   <array>[<index>]
    * }}}
    *
    * @param array target array
    * @param index index to be selected, starting from 0
    */
  case class IndexSel(array: Expr, index: Expr) extends Expr

  /**
    * This expression:
    * {{{
    *   this
    * }}}
    *
    * Refers to the instance of the current class.
    */
  case class This() extends Expr

  /**
    * Call expression:
    * {{{
    *   [<receiver>.]<id>(<arg1>, <arg2>, ...)
    * }}}
    *
    * @param receiver target instance, `this` if not given
    * @param method   identifier of the selected method
    * @param args     arguments
    */
  case class Call(receiver: Option[Expr], method: Id, args: List[Expr]) extends Expr

  def Call(receiver: Expr, method: Id, args: List[Expr]): Call = Call(Some(receiver), method, args)

  def Call(method: Id, args: List[Expr]): Call = Call(None, method, args)

  /**
    * Unary expression.
    *
    * @param op   unary operator
    * @param expr operand
    */
  case class UnaryExpr(op: UnaryOp, expr: Expr) extends Expr

  /**
    * Binary expression.
    *
    * @param op  binary operator
    * @param lhs left operand
    * @param rhs right operand
    */
  case class BinaryExpr(op: BinaryOp, lhs: Expr, rhs: Expr) extends Expr

  /**
    * Operator.
    */
  abstract class Op extends Tree

  /**
    * Unary operator.
    */
  abstract class UnaryOp extends Op

  /**
    * Integer negation: {{{ - }}}.
    */
  case class NEG() extends UnaryOp

  /**
    * Logical negation: {{{ ! }}}.
    */
  case class NOT() extends UnaryOp

  abstract class BinaryOp extends Op

  /**
    * Addition: {{{ + }}}.
    */
  case class ADD() extends BinaryOp

  /**
    * Subtraction: {{{ - }}}.
    */
  case class SUB() extends BinaryOp

  /**
    * Multiplication: {{{ * }}}.
    */
  case class MUL() extends BinaryOp

  /**
    * Division: {{{ / }}}.
    */
  case class DIV() extends BinaryOp

  /**
    * Modulo: {{{ % }}}.
    */
  case class MOD() extends BinaryOp

  /**
    * Logical and: {{{ && }}}.
    */
  case class AND() extends BinaryOp

  /**
    * Logical or: {{{ || }}}.
    */
  case class OR() extends BinaryOp

  /**
    * Equal to: {{{ == }}}.
    */
  case class EQ() extends BinaryOp

  /**
    * Not equal to: {{{ != }}}.
    */
  case class NE() extends BinaryOp

  /**
    * Less than: {{{ < }}}.
    */
  case class LT() extends BinaryOp

  /**
    * Less than or equal to: {{{ <= }}}.
    */
  case class LE() extends BinaryOp

  /**
    * Greater than: {{{ > }}}.
    */
  case class GT() extends BinaryOp

  /**
    * Greater than or equal to: {{{ >= }}}.
    */
  case class GE() extends BinaryOp

  /**
    * IO expression for reading an integer from stdin:
    * {{{
    *   ReadInteger()
    * }}}
    */
  case class ReadInt() extends Expr

  /**
    * IO expression for reading a line from stdin:
    * {{{
    *   ReadLine()
    * }}}
    */
  case class ReadLine() extends Expr

  /**
    * New expression for creating an instance:
    * {{{
    *   new <id>()
    * }}}
    *
    * @param id class identifier
    */
  case class NewClass(id: Id) extends Expr

  /**
    * New expression for creating an array:
    * {{{
    *   new <elemType>[<length>]
    * }}}
    *
    * @param elemType array element type
    * @param length   array length
    */
  case class NewArray(elemType: Type, length: Expr) extends Expr

  /**
    * Instance-of-expression:
    * {{{
    *   instanceof(<obj>, <is>)
    * }}}
    * Check if the given object `obj` is an instance of class `is`.
    */
  case class ClassTest(obj: Expr, is: Id) extends Expr

  /**
    * Class type cast expression:
    * {{{
    *   (class <to>)obj
    * }}}
    * Cast the given object `obj` into class type `to`.
    */
  case class ClassCast(obj: Expr, to: Id) extends Expr

  /**
    * Identifier.
    *
    * @param name its name
    */
  case class Id(name: String) extends Expr

}
