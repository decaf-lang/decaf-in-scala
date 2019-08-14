package decaf.frontend.tree

import decaf.frontend.annot._

object NamedTree extends TopLevelTmpl {

  type TopLevelAnnot = GlobalScope
  type ClassAnnot = ClassSymbol
  type MemberVarAnnot = MemberVarSymbol
  type MethodAnnot = MethodSymbol
  type ClassRef = ClassSymbol

  override type Stmt = SyntaxTree.Stmt

  override type TypeLit = TypedTree.TypeLit
  override type LocalVarDef = TypedTree.LocalVarDef

}