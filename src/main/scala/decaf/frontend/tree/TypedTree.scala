package decaf.frontend.tree

import decaf.frontend.annot._
import decaf.frontend.tree.TreeNode.Id

object TypedTree extends TreeLevelTmpl with BlockLevelTmpl {

  type TopLevelAnnot = GlobalScope
  type ClassDefAnnot = ClassSymbol
  type VarDefAnnot = VarSymbol
  type MethodDefAnnot = FunSymbol
  type TypeLitAnnot = Type
  type BlockAnnot = SyntaxTree.No
  type ControlFlowStmtAnnot = SyntaxTree.No
  type ExprAnnot = Type

  case class TypeVar(id: Id, annot: Type) extends Expr

}
