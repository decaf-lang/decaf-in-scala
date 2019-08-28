package decaf.tree

import decaf.annot._

/**
  * In a named tree:
  * - The root is labeled with a `GlobalScope`
  * - Every class is labeled with a `ClassSymbol`
  * - Every method is labeled with a `MethodSymbol`
  * - Every member variable is labeled with a `MemberVarSymbol`
  * - Every method body is not yet resolved and thus remains `SyntaxTree.Stmt`
  */
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