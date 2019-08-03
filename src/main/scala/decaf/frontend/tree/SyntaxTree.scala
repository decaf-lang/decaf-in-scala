package decaf.frontend.tree

object SyntaxTree {

  type Id = TreeNode.Id
  type Def = TreeNode.Def

  // top level
  type Tree = TreeNode.TopLevel[ClassDef, Null]
  type ClassDef = TreeNode.ClassDef[Field, Null]

  // fields
  trait Field extends TreeNode.FieldNode

  case class VarDef(typeLit: TypeLit, id: Id) extends Field with TreeNode.VarDefTmpl[TypeLit, Null] {
    override val annot: Null = null
  }

  case class MethodDef(isStatic: Boolean, returnType: TypeLit, id: Id, params: List[VarDef], body: Block)
    extends Field with TreeNode.MethodDefTmpl[TypeLit, VarDef, Block, Null] {
    override val annot: Null = null
  }

  // types
  type TypeLit = TreeNode.TypeLit[Null]
  type TInt = TreeNode.TInt[Null]
  type TBool = TreeNode.TBool[Null]
  type TString = TreeNode.TString[Null]
  type TVoid = TreeNode.TVoid[Null]
  type TClass = TreeNode.TClass[Null]
  type TArray = TreeNode.TArray[Null]

  // statements
  trait Stmt extends TreeNode.StmtNode

  case class LocalVarDef(typeLit: TypeLit, id: Id) extends Stmt with TreeNode.LocalVarDefTmpl[TypeLit, Null] {
    override val annot: Null = null
  }

  case class Block(stmts: List[Stmt]) extends Stmt with TreeNode.BlockTmpl[Stmt, Null] {
    override val annot: Null = null
  }

  case class ControlStmtWarp(self: ControlStmt) extends Stmt with TreeNode.ControlStmtTmpl[Null]

  type ControlStmt = TreeNode.ControlStmt[Null]

  type SimpleStmt = TreeNode.SimpleStmt[Null]
  type Assign = TreeNode.Assign[Expr, Null]
  type ExprEval = TreeNode.ExprEval[Expr, Null]
  type Skip = TreeNode.Skip[Null]
  type If = TreeNode.If[Expr, Stmt, Null]
  type While = TreeNode.While[Expr, Stmt, Null]
  type For = TreeNode.For[Expr, Stmt, Null]
  type Break = TreeNode.Break[Null]
  type Return = TreeNode.Return[Expr, Null]
  type Print = TreeNode.Print[Expr, Null]

  // expressions
  type Expr = TreeNode.Expr[Null]
  type Lit = TreeNode.Lit[Null]
  type IntLit = TreeNode.IntLit[Null]
  type BoolLit = TreeNode.BoolLit[Null]
  type StringLit = TreeNode.StringLit[Null]
  type NullLit = TreeNode.NullLit[Null]
  type Var = TreeNode.Var[Null]
  type FieldSel = TreeNode.FieldSel[Null]
  type IndexSel = TreeNode.IndexSel[Null]
  type This = TreeNode.This[Null]
  type Call = TreeNode.Call[Null]
  type UnaryExpr = TreeNode.UnaryExpr[Null]
  type BinaryExpr = TreeNode.BinaryExpr[Null]
  type ReadInt = TreeNode.ReadInt[Null]
  type ReadLine = TreeNode.ReadLine[Null]
  type NewClass = TreeNode.NewClass[Null]
  type NewArray = TreeNode.NewArray[TypeLit, Null]
  type ClassTest = TreeNode.ClassTest[Null]
  type ClassCast = TreeNode.ClassCast[Null]
}
