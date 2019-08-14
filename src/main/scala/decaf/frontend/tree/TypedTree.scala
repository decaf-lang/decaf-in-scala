package decaf.frontend.tree

import decaf.frontend.annot._

object TypedTree extends TreeTmpl {

  type TopLevelAnnot = GlobalScope
  type ClassAnnot = ClassSymbol
  type MemberVarAnnot = MemberVarSymbol
  type LocalVarAnnot = LocalVarSymbol
  type MethodAnnot = MethodSymbol
  type TypeLitAnnot = Type
  type StmtAnnot = SyntaxTree.No
  type ExprAnnot = Type

  type ClassRef = ClassSymbol

  // The following nodes only appear in a typed tree.

  case class LocalVar(variable: LocalVarSymbol)(implicit val annot: ExprAnnot) extends LValue

  case class MemberVar(receiver: Expr, variable: MemberVarSymbol)(implicit val annot: ExprAnnot) extends LValue

  case class StaticCall(method: MethodSymbol, args: List[Expr])(implicit val annot: ExprAnnot) extends Expr

  case class MemberCall(receiver: Expr, method: MethodSymbol, args: List[Expr])(implicit val annot: ExprAnnot)
    extends Expr

  case class ArrayLen(array: Expr)(implicit val annot: ExprAnnot) extends Expr

  case class Ill(expr: SyntaxTree.Expr)(implicit val annot: ExprAnnot) extends LValue

}
