package decaf.frontend.tree

import decaf.frontend.annot.{Annot, Annotated}

import scala.util.parsing.input.Positional

object TreeNode {

  trait Node extends Product with Positional

  /**
    * A top-level decaf program, which consists of many class definitions.
    */
  case class TopLevel[ClassDef <: ClassDefNode, A <: Annot](classes: List[ClassDef], override val annot: A = null)
    extends Node with Annotated[A]

  /**
    * Any definition must be named.
    */
  trait Def extends Node {
    val id: Id
    val name: String = id.name
  }

  trait ClassDefNode extends Node

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
  case class ClassDef[Field <: FieldNode, A <: Annot](id: Id, parent: Option[Id], fields: List[Field],
                                                      override val annot: A = null)
    extends ClassDefNode with Annotated[A] with Def {
    def parentDetached: ClassDef[Field, A] = ClassDef(id, None, fields, annot).setPos(pos)
  }

  /**
    * Field/Member of a class.
    */
  trait FieldNode extends Def

  trait VarDefNode extends FieldNode

  /**
    * Member variable declaration:
    * {{{
    *  <typ> <id>;
    * }}}
    * Initialization is not supported.
    */
  trait VarDefTmpl[TypeLit <: TypeLitNode, A <: Annot] extends VarDefNode with Annotated[A] {
    val typeLit: TypeLit
    val id: Id
  }

  /**
    * Member method definition:
    * {{{
    *   [static] <returnType> <id> (<typ1> <id1>, <typ2> <id2>, ...) { <body> }
    * }}}
    * Decaf has static methods but _no_ static variables, strange!
    */
  trait MethodDefTmpl[TypeLit <: TypeLitNode, VarDef <: VarDefNode, Block <: BlockNode, A <: Annot]
    extends FieldNode with Annotated[A] {
    /**
      * Is this method static?
      */
    val isStatic: Boolean

    /**
      * Return type.
      */
    val returnType: TypeLit

    /**
      * Method identifier.
      */
    val id: Id

    /**
      * Parameters, each is a typed identifier (or _formal_, as said in the language specification).
      */
    val params: List[VarDef]

    /**
      * Method body (a statement block).
      */
    val body: Block
  }

  /**
    * Type. Decaf only supports
    * - basic types (integer, boolean, string, void),
    * - class types (using class identifiers), and
    * - array types (whose element could be any type, but homogeneous).
    */
  trait TypeLitNode extends Node

  trait TypeLit[A <: Annot] extends TypeLitNode with Annotated[A]

  /**
    * 32 bit integer type: {{{ int }}}
    */
  case class TInt[A <: Annot](override val annot: A = null) extends TypeLit[A]

  /**
    * Boolean type: {{{ bool }}}
    */
  case class TBool[A <: Annot](override val annot: A = null) extends TypeLit[A]

  /**
    * String type: {{{ string }}}
    */
  case class TString[A <: Annot](override val annot: A = null) extends TypeLit[A]

  /**
    * Void type: {{{ void }}}
    * For method return type _only_.
    */
  case class TVoid[A <: Annot](override val annot: A = null) extends TypeLit[A]

  /**
    * Class type:
    * {{{
    *   class <id>
    * }}}
    *
    * @param id class identifier
    */
  case class TClass[A <: Annot](id: Id, override val annot: A = null) extends TypeLit[A]

  /**
    * Array type:
    * {{{
    *   <elemType>[]
    * }}}
    *
    * @param elemType element type
    */
  case class TArray[A <: Annot](elemType: TypeLit[A], override val annot: A = null) extends TypeLit[A]

  /**
    * Statement.
    */
  trait StmtNode extends Node

  /**
    * Local variable declaration:
    * {{{
    *  <typeLit> <id>;
    * }}}
    * Initialization is not supported.
    */
  trait LocalVarDefTmpl[TypeLit <: TypeLitNode, A <: Annot] extends StmtNode with Def with Annotated[A] {
    val typeLit: TypeLit
    val id: Id
  }

  trait BlockNode extends StmtNode

  /**
    * Statement block:
    * {{{
    *   { <stmt1> <stmt2> ... }
    * }}}
    */
  trait BlockTmpl[Stmt <: StmtNode, A <: Annot] extends BlockNode with Annotated[A] {
    val stmts: List[Stmt]
  }

  /**
    * Control flow statements.
    *
    * They should have the same type of annotation in a specialized tree.
    */
  trait ControlStmt[A <: Annot] extends StmtNode with Annotated[A]

  trait ControlStmtTmpl[A <: Annot] extends StmtNode {
    val self: ControlStmt[A]
  }

  /**
    * Simple statement.
    */
  trait SimpleStmt[A <: Annot] extends ControlStmt[A]

  /**
    * Assignment:
    * {{{
    *   <lhs> = <rhs>;
    * }}}
    *
    * @param lhs left hand side, i.e. the left-value to be assigned
    * @param rhs right hand side, i.e. the value to assign
    */
  case class Assign[Expr <: ExprNode, A <: Annot](lhs: Expr, rhs: Expr, override val annot: A = null)
    extends SimpleStmt[A]

  /**
    * Expression evaluation, typically a method call.
    *
    * @param expr expression to be evaluated
    */
  case class ExprEval[Expr <: ExprNode, A <: Annot](expr: Expr, override val annot: A = null) extends SimpleStmt[A]

  /**
    * Empty statement, do nothing.
    */
  case class Skip[A <: Annot](override val annot: A = null) extends SimpleStmt[A]

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
  case class If[Expr <: ExprNode, Stmt <: StmtNode, A <: Annot](cond: Expr, trueBranch: Stmt, falseBranch: Option[Stmt],
                                                                override val annot: A = null) extends ControlStmt[A]

  /**
    * While statement:
    * {{{
    *   while (<cond>) <body>
    * }}}
    *
    * @param cond condition
    * @param body loop body to execute if condition holds
    */
  case class While[Expr <: ExprNode, Stmt <: StmtNode, A <: Annot](cond: Expr, body: Stmt,
                                                                   override val annot: A = null) extends ControlStmt[A]

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
  case class For[Expr <: ExprNode, Stmt <: StmtNode, A <: Annot](init: SimpleStmt[A], cond: Expr, update: SimpleStmt[A],
                                                                 body: Stmt, override val annot: A = null)
    extends ControlStmt[A]

  /**
    * Break statement:
    * {{{
    *   break;
    * }}}
    *
    * Jump out of the _innermost_ loop.
    */
  case class Break[A <: Annot](override val annot: A = null) extends ControlStmt[A]

  /**
    * Method return statement:
    * {{{
    *   return [<expr>];
    * }}}
    *
    * @param expr value to be returned, or nothing if not given (in this case, the method return type must be void)
    */
  case class Return[Expr <: ExprNode, A <: Annot](expr: Option[Expr], override val annot: A = null)
    extends ControlStmt[A]

  /**
    * Print statement:
    * {{{
    *   Print(<expr1>, <expr2>, ...);
    * }}}
    *
    * @param exprs values to be printed
    */
  case class Print[Expr <: ExprNode, A <: Annot](exprs: List[Expr], override val annot: A = null)
    extends ControlStmt[A]

  /**
    * Expression.
    */
  trait ExprNode extends Node

  trait Expr[A <: Annot] extends ExprNode with Annotated[A]

  /**
    * Literal/Constant.
    */
  trait Lit[A <: Annot] extends Expr[A] {
    type T
    val value: T
  }

  /**
    * Integer literal.
    */
  case class IntLit[A <: Annot](value: Int, override val annot: A = null) extends Lit[A] {
    type T = Int
  }

  /**
    * Boolean literal.
    */
  case class BoolLit[A <: Annot](value: Boolean, override val annot: A = null) extends Lit[A] {
    type T = Boolean
  }

  /**
    * String literal.
    */
  case class StringLit[A <: Annot](value: String, override val annot: A = null) extends Lit[A] {
    type T = String
  }

  /**
    * Null literal: {{{ null }}}
    */
  case class NullLit[A <: Annot](override val annot: A = null) extends Lit[A] {
    type T = Null

    val value = null
  }

  /**
    * Variable.
    *
    * @param id identifier
    */
  case class Var[A <: Annot](id: Id, override val annot: A = null) extends Expr[A]

  /**
    * Field selection:
    * {{{
    *   <receiver>.<field>
    * }}}
    *
    * @param receiver target instance
    * @param field    identifier of the selected field
    */
  case class FieldSel[A <: Annot](receiver: Expr[A], field: Id, override val annot: A = null) extends Expr[A]

  /**
    * Array element selection by index:
    * {{{
    *   <array>[<index>]
    * }}}
    *
    * @param array target array
    * @param index index to be selected, starting from 0
    */
  case class IndexSel[A <: Annot](array: Expr[A], index: Expr[A], override val annot: A = null) extends Expr[A]

  /**
    * This expression:
    * {{{
    *   this
    * }}}
    *
    * Refers to the instance of the current class.
    */
  case class This[A <: Annot](override val annot: A = null) extends Expr[A]

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
  case class Call[A <: Annot](receiver: Option[Expr[A]], method: Id, args: List[Expr[A]], override val annot: A = null)
    extends Expr[A]

  /**
    * Unary expression.
    *
    * @param op      unary operator
    * @param operand operand
    */
  case class UnaryExpr[A <: Annot](op: UnaryOp, operand: Expr[A], override val annot: A = null) extends Expr[A]

  /**
    * Binary expression.
    *
    * @param op  binary operator
    * @param lhs left operand
    * @param rhs right operand
    */
  case class BinaryExpr[A <: Annot](op: BinaryOp, lhs: Expr[A], rhs: Expr[A], override val annot: A = null)
    extends Expr[A]

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

  /**
    * IO expression for reading an integer from stdin:
    * {{{
    *   ReadInteger()
    * }}}
    */
  case class ReadInt[A <: Annot](override val annot: A = null) extends Expr[A]

  /**
    * IO expression for reading a line from stdin:
    * {{{
    *   ReadLine()
    * }}}
    */
  case class ReadLine[A <: Annot](override val annot: A = null) extends Expr[A]

  /**
    * New expression for creating an instance:
    * {{{
    *   new <id>()
    * }}}
    *
    * @param id class identifier
    */
  case class NewClass[A <: Annot](id: Id, override val annot: A = null) extends Expr[A]

  /**
    * New expression for creating an array:
    * {{{
    *   new <elemType>[<length>]
    * }}}
    *
    * @param elemType array element type
    * @param length   array length
    */
  case class NewArray[A <: Annot](elemType: TypeLit[A], length: Expr[A], override val annot: A = null) extends Expr[A]

  /**
    * Instance-of-expression:
    * {{{
    *   instanceof(<obj>, <is>)
    * }}}
    * Check if the given object `obj` is an instance of class `is`.
    */
  case class ClassTest[A <: Annot](obj: Expr[A], is: Id, override val annot: A = null) extends Expr[A]

  /**
    * Class type cast expression:
    * {{{
    *   (class <to>)obj
    * }}}
    * Cast the given object `obj` into class type `to`.
    */
  case class ClassCast[A <: Annot](obj: Expr[A], to: Id, override val annot: A = null) extends Expr[A]

  /**
    * Identifier.
    *
    * @param name its name
    */
  case class Id(name: String) extends Node

}