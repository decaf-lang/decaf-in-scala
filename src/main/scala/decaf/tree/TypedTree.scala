package decaf.tree

import decaf.annot._

/**
  * Compared to the NamedTree, a TypedTree is fully resolved and:
  * - Every type literal is labeled with the actual type (i.e. of type `decaf.annot.Type`)
  * - Every expression is labeled with its type
  * - Every statement is type checked
  * - Every reference (e.g. refers a class, method, local variable) is linked to a symbol of the element that it points
  * to
  */
object TypedTree extends TreeTmpl {

  type TopLevelAnnot = GlobalScope
  type ClassAnnot = ClassSymbol
  type MemberVarAnnot = MemberVarSymbol
  type LocalVarAnnot = LocalVarSymbol
  type MethodAnnot = MethodSymbol
  type TypeLitAnnot = Type
  type StmtAnnot = SyntaxTree.No
  type BlockAnnot = LocalScope
  type ExprAnnot = Type

  type ClassRef = ClassSymbol

  // The following nodes only appear in a typed tree.

  /**
    * A local variable. Derives from a `VarSel` in syntax tree.
    *
    * @param variable the symbol of the variable definition it refers to
    */
  case class LocalVar(variable: LocalVarSymbol)(implicit val annot: ExprAnnot) extends LValue

  /**
    * A member variable. Derives from a `VarSel` in syntax tree.
    *
    * @param receiver target instance
    * @param variable the symbol of the member definition it refers to
    */
  case class MemberVar(receiver: Expr, variable: MemberVarSymbol)(implicit val annot: ExprAnnot) extends LValue

  /**
    * Calling a static method. Derives from a `Call` in syntax tree.
    *
    * @param method the symbol of the method definition it invokes
    * @param args   arguments
    */
  case class StaticCall(method: MethodSymbol, args: List[Expr])(implicit val annot: ExprAnnot) extends Expr

  /**
    * Calling a member/instance method. Derives from a `Call` in syntax tree.
    *
    * @param receiver target instance
    * @param method   the symbol of the method definition it invokes
    * @param args     arguments
    */
  case class MemberCall(receiver: Expr, method: MethodSymbol, args: List[Expr])(implicit val annot: ExprAnnot)
    extends Expr

  /**
    * Obtain array length.
    *
    * @param array the array
    */
  case class ArrayLen(array: Expr)(implicit val annot: ExprAnnot) extends Expr

  // Black magic to handle expressions that are not typed yet!

  case class UntypedExpr(expr: SyntaxTree.Expr)(implicit val annot: ExprAnnot = NoType) extends Expr

  implicit def syntaxTreeExprAsUntyped(expr: SyntaxTree.Expr): UntypedExpr = UntypedExpr(expr).setPos(expr.pos)

  implicit def syntaxTreeExprListAsUntyped(exprs: List[SyntaxTree.Expr]): List[UntypedExpr] =
    exprs.map { e => UntypedExpr(e).setPos(e.pos) }

  implicit def syntaxTreeExprOptionAsUntyped(expr: Option[SyntaxTree.Expr]): Option[UntypedExpr] =
    expr.map { e => UntypedExpr(e).setPos(e.pos) }

  case class UntypedLValue(expr: SyntaxTree.LValue)(implicit val annot: ExprAnnot = NoType) extends LValue

  implicit def syntaxTreeLValueAsUntyped(expr: SyntaxTree.LValue): UntypedLValue = UntypedLValue(expr)

  implicit def typedExprAsSyntaxTreeExpr(wrap: Expr): SyntaxTree.Expr = {
    wrap.asInstanceOf[UntypedExpr].expr
  }

  implicit def typedLValueAsSyntaxTreeLValue(wrap: LValue): SyntaxTree.LValue = {
    wrap.asInstanceOf[UntypedLValue].expr
  }
}
