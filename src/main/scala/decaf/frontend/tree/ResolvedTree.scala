package decaf.frontend.tree

import decaf.frontend.annot.{ClassSymbol, FunSymbol, GlobalScope, Type, VarSymbol}

object ResolvedTree extends TreeLevelTmpl {

  type TopLevelAnnot = GlobalScope
  type ClassDefAnnot = ClassSymbol
  type VarDefAnnot = VarSymbol
  type MethodDefAnnot = FunSymbol

  type TypeLit = TypedTree.TypeLit
  type Block = SyntaxTree.Block
}
