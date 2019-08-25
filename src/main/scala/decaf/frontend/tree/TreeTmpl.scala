package decaf.frontend.tree

import decaf.frontend.annot.{Annot, Annotated}
import decaf.frontend.tree.TreeNode._

trait TopLevelTmpl {

  /**
    * Let the compiler developer specify which annotations they need for different categories of trees.
    */
  type TopLevelAnnot <: Annot
  type ClassAnnot <: Annot
  type MemberVarAnnot <: Annot
  type MethodAnnot <: Annot

  type ClassRef

  type Tree = TopLevel

  /**
    * A top-level decaf program, which consists of many class definitions.
    */
  case class TopLevel(classes: List[ClassDef])(implicit val annot: TopLevelAnnot)
    extends Node with Annotated[TopLevelAnnot]

  /**
    * Class definition:
    * {{{
    *   class <id> [extends <parent>] { <fields> }
    * }}}
    *
    * @param id     class identifier
    * @param parent parent class
    * @param fields fields/members
    */
  case class ClassDef(id: Id, parent: Option[ClassRef], fields: List[Field])(implicit val annot: ClassAnnot)
    extends Def with Annotated[ClassAnnot] {
    def parentDetached: ClassDef = ClassDef(id, None, fields)(annot).setPos(pos)

    def methods: List[MethodDef] = fields.flatMap {
      case m: MethodDef => Some(m)
      case _ => None
    }
  }

  /**
    * Field/Member of a class.
    */
  trait Field extends Def

  type TypeLit <: Node

  /**
    * Member variable declaration:
    * {{{
    *  <typ> <id>;
    * }}}
    * Initialization is not supported.
    */
  case class VarDef(typeLit: TypeLit, id: Id)(implicit val annot: MemberVarAnnot)
    extends Field with Var with Annotated[MemberVarAnnot] {
    type TypeLitT = TypeLit
  }

  type Stmt <: Node

  type LocalVarDef <: Node

  /**
    * Member method definition:
    * {{{
    *   [static] <returnType> <id> (<typ1> <id1>, <typ2> <id2>, ...) { <body> }
    * }}}
    * Decaf has static methods but _no_ static variables, strange!
    *
    * @param isStatic   is this method static?
    * @param returnType return type
    * @param id         method identifier
    * @param params     parameters, each is a typed identifier (or _formal_, as said in the language specification)
    * @param body       method body (a statement block, by syntactic grammar)
    *
    */
  case class MethodDef(isStatic: Boolean, returnType: TypeLit, id: Id, params: List[LocalVarDef], body: Stmt)
                      (implicit val annot: MethodAnnot) extends Field with Annotated[MethodAnnot]

}

trait TreeTmpl extends TopLevelTmpl {

  /**
    * Let the compiler developer specify which annotations they need for different categories of trees.
    */
  type LocalVarAnnot <: Annot
  type TypeLitAnnot <: Annot
  type StmtAnnot <: Annot
  type ExprAnnot <: Annot

  /**
    * Type. Decaf only supports
    * - basic types (integer, boolean, string, void),
    * - class types (using class identifiers), and
    * - array types (whose element could be any type, but homogeneous).
    */
  trait TypeLit extends Node with Annotated[TypeLitAnnot]

  /**
    * 32 bit integer type: {{{ int }}}
    */
  case class TInt()(implicit val annot: TypeLitAnnot) extends TypeLit

  /**
    * Boolean type: {{{ bool }}}
    */
  case class TBool()(implicit val annot: TypeLitAnnot) extends TypeLit

  /**
    * String type: {{{ string }}}
    */
  case class TString()(implicit val annot: TypeLitAnnot) extends TypeLit

  /**
    * Void type: {{{ void }}}
    * For method return type _only_.
    */
  case class TVoid()(implicit val annot: TypeLitAnnot) extends TypeLit

  /**
    * Class type:
    * {{{
    *   class <id>
    * }}}
    *
    * @param id class
    */
  case class TClass(id: ClassRef)(implicit val annot: TypeLitAnnot) extends TypeLit

  /**
    * Array type:
    * {{{
    *   <elemType>[]
    * }}}
    *
    * @param elemType element type
    */
  case class TArray(elemType: TypeLit)(implicit val annot: TypeLitAnnot) extends TypeLit

  /**
    * Statement.
    */
  trait Stmt extends Node {
    def isEmpty: Boolean = false
  }

  /**
    * Local variable declaration:
    * {{{
    *  <typeLit> <id>;
    * }}}
    * Initialization is not supported.
    */
  case class LocalVarDef(typeLit: TypeLit, id: Id)(implicit val annot: LocalVarAnnot)
    extends Stmt with Var with Annotated[LocalVarAnnot] {
    type TypeLitT = TypeLit
  }

  /**
    * Normal control flow statements.
    *
    * They should have the same type of annotation in a specialized tree.
    */
  trait ControlFlowStmt extends Stmt with Annotated[StmtAnnot]

  /**
    * Statement block:
    * {{{
    *   { <stmt1> <stmt2> ... }
    * }}}
    */
  case class Block(stmts: List[Stmt] = Nil)(implicit val annot: StmtAnnot) extends ControlFlowStmt {
    override def isEmpty: Boolean = stmts.isEmpty
  }

  /**
    * Simple statement.
    */
  trait SimpleStmt extends ControlFlowStmt

  /**
    * Assignment:
    * {{{
    *   <lhs> = <rhs>;
    * }}}
    *
    * @param lhs left hand side, i.e. the left-value to be assigned
    * @param rhs right hand side, i.e. the value to assign
    */
  case class Assign(lhs: LValue, rhs: Expr)(implicit val annot: StmtAnnot) extends SimpleStmt

  /**
    * Expression evaluation, typically a method call.
    *
    * @param expr expression to be evaluated
    */
  case class ExprEval(expr: Expr)(implicit val annot: StmtAnnot) extends SimpleStmt

  /**
    * Empty statement, do nothing.
    */
  case class Skip()(implicit val annot: StmtAnnot) extends SimpleStmt {
    override def isEmpty: Boolean = true
  }

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
  case class If(cond: Expr, trueBranch: Stmt, falseBranch: Stmt)(implicit val annot: StmtAnnot)
    extends ControlFlowStmt

  /**
    * While statement:
    * {{{
    *   while (<cond>) <body>
    * }}}
    *
    * @param cond condition
    * @param body loop body to execute if condition holds
    */
  case class While(cond: Expr, body: Stmt)(implicit val annot: StmtAnnot) extends ControlFlowStmt

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
  case class For(init: Stmt, cond: Expr, update: Stmt, body: Stmt)(implicit val annot: StmtAnnot)
    extends ControlFlowStmt

  /**
    * Break statement:
    * {{{
    *   break;
    * }}}
    *
    * Jump out of the _innermost_ loop.
    */
  case class Break()(implicit val annot: StmtAnnot) extends ControlFlowStmt

  /**
    * Method return statement:
    * {{{
    *   return [<expr>];
    * }}}
    *
    * @param expr value to be returned, or nothing if not given (in this case, the method return type must be void)
    */
  case class Return(expr: Option[Expr])(implicit val annot: StmtAnnot) extends ControlFlowStmt

  /**
    * Print statement:
    * {{{
    *   Print(<expr1>, <expr2>, ...);
    * }}}
    *
    * @param exprs values to be printed
    */
  case class Print(exprs: List[Expr])(implicit val annot: StmtAnnot) extends ControlFlowStmt

  /**
    * Expression.
    */
  trait Expr extends Node with Annotated[ExprAnnot]

  /**
    * Literal/Constant.
    */
  trait Lit extends Expr {
    type T
    val value: T
  }

  /**
    * Integer literal.
    */
  case class IntLit(value: Int)(implicit val annot: ExprAnnot) extends Lit {
    type T = Int
  }

  /**
    * Boolean literal.
    */
  case class BoolLit(value: Boolean)(implicit val annot: ExprAnnot) extends Lit {
    type T = Boolean
  }

  /**
    * String literal.
    */
  case class StringLit(value: String)(implicit val annot: ExprAnnot) extends Lit {
    type T = String
  }

  /**
    * Null literal: {{{ null }}}
    */
  case class NullLit()(implicit val annot: ExprAnnot) extends Lit {
    type T = Null

    val value = null
  }

  trait LValue extends Expr

  /**
    * Array element selection by index:
    * {{{
    *   <array>[<index>]
    * }}}
    *
    * @param array target array
    * @param index index to be selected, starting from 0
    */
  case class IndexSel(array: Expr, index: Expr)(implicit val annot: ExprAnnot) extends LValue

  /**
    * This expression:
    * {{{
    *   this
    * }}}
    *
    * Refers to the instance of the current class.
    */
  case class This()(implicit val annot: ExprAnnot) extends Expr

  /**
    * Unary expression.
    *
    * @param op      unary operator
    * @param operand operand
    */
  case class UnaryExpr(op: Op, operand: Expr)(implicit val annot: ExprAnnot) extends Expr

  /**
    * Binary expression.
    *
    * @param op  binary operator
    * @param lhs left operand
    * @param rhs right operand
    */
  case class BinaryExpr(op: Op, lhs: Expr, rhs: Expr)(implicit val annot: ExprAnnot) extends Expr

  /**
    * IO expression for reading an integer from stdin:
    * {{{
    *   ReadInteger()
    * }}}
    */
  case class ReadInt()(implicit val annot: ExprAnnot) extends Expr

  /**
    * IO expression for reading a line from stdin:
    * {{{
    *   ReadLine()
    * }}}
    */
  case class ReadLine()(implicit val annot: ExprAnnot) extends Expr

  /**
    * New expression for creating an instance:
    * {{{
    *   new <id>()
    * }}}
    *
    * @param clazz class
    */
  case class NewClass(clazz: ClassRef)(implicit val annot: ExprAnnot) extends Expr

  /**
    * New expression for creating an array:
    * {{{
    *   new <elemType>[<length>]
    * }}}
    *
    * @param elemType array element type
    * @param length   array length
    */
  case class NewArray(elemType: TypeLit, length: Expr)(implicit val annot: ExprAnnot) extends Expr

  /**
    * Instance-of-expression:
    * {{{
    *   instanceof(<obj>, <is>)
    * }}}
    * Check if the given object `obj` is an instance of class `is`.
    */
  case class ClassTest(obj: Expr, is: ClassRef)(implicit val annot: ExprAnnot) extends Expr

  /**
    * Class type cast expression:
    * {{{
    *   (class <to>)obj
    * }}}
    * Cast the given object `obj` into class type `to`.
    */
  case class ClassCast(obj: Expr, to: ClassRef)(implicit val annot: ExprAnnot) extends Expr

}
