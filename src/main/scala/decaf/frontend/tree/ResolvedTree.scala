package decaf.frontend.tree

import decaf.frontend.annot.{ClassSymbol, FunSymbol, GlobalScope, VarSymbol}

object ResolvedTree {

  type Id = TreeNode.Id

  // top level
  type Tree = TreeNode.TopLevel[ClassDef, GlobalScope]
  type ClassDef = TreeNode.ClassDef[Field, ClassSymbol]

  // fields
  sealed trait Field extends TreeNode.FieldNode

  case class VarDef(typeLit: TypeLit, id: Id, annot: VarSymbol)
    extends Field with TreeNode.VarDefTmpl[TypeLit, VarSymbol]

  case class MethodDef(isStatic: Boolean, returnType: TypeLit, id: Id, params: List[VarDef], body: Block)
    extends Field with TreeNode.MethodDefTmpl[TypeLit, VarDef, Block, FunSymbol] {
    override val annot: Null = null
  }

  // types
  type TypeLit = TypedTree.TypeLit

  // blocks are untouched, thus same in SyntaxTree
  type Block = SyntaxTree.Block
}
