package decaf.frontend.tree

import decaf.frontend.annot._

object TypedTree {

  type Id = TreeNode.Id

  // top level
  type Tree = TreeNode.TopLevel[ClassDef, GlobalScope]
  type ClassDef = TreeNode.ClassDef[Field, ClassSymbol]

  // fields
  trait Field extends TreeNode.FieldNode

  case class VarDef(typeLit: TypeLit, id: Id, annot: VarSymbol)
    extends Field with TreeNode.VarDefTmpl[TypeLit, VarSymbol]

  case class MethodDef(isStatic: Boolean, returnType: TypeLit, id: Id, params: List[VarDef], body: Block)
    extends Field with TreeNode.MethodDefTmpl[TypeLit, VarDef, Block, FunSymbol] {
    override val annot: Null = null
  }

  // types
  type TypeLit = TreeNode.TypeLit[Type]
  type TInt = TreeNode.TInt[Type]
  type TBool = TreeNode.TBool[Type]
  type TString = TreeNode.TString[Type]
  type TVoid = TreeNode.TVoid[Type]
  type TClass = TreeNode.TClass[Type]
  type TArray = TreeNode.TArray[Type]

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
  type Expr = TreeNode.Expr[Type]
  type Lit = TreeNode.Lit[Type]
  type IntLit = TreeNode.IntLit[Type]
  type BoolLit = TreeNode.BoolLit[Type]
  type StringLit = TreeNode.StringLit[Type]
  type NullLit = TreeNode.NullLit[Type]
  type Var = TreeNode.Var[Type]
  type FieldSel = TreeNode.FieldSel[Type]
  type IndexSel = TreeNode.IndexSel[Type]
  type This = TreeNode.This[Type]
  type Call = TreeNode.Call[Type]
  type UnaryExpr = TreeNode.UnaryExpr[Type]
  type BinaryExpr = TreeNode.BinaryExpr[Type]
  type ReadInt = TreeNode.ReadInt[Type]
  type ReadLine = TreeNode.ReadLine[Type]
  type NewClass = TreeNode.NewClass[Type]
  type NewArray = TreeNode.NewArray[Type]
  type ClassTest = TreeNode.ClassTest[Type]
  type ClassCast = TreeNode.ClassCast[Type]
}
