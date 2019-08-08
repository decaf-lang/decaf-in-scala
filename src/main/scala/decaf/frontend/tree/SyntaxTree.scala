package decaf.frontend.tree

import decaf.frontend.annot.Annot

object SyntaxTree extends TreeLevelTmpl with BlockLevelTmpl {

  implicit object NoAnnot extends Annot

  type No = NoAnnot.type

  type TopLevelAnnot = No
  type ClassDefAnnot = No
  type VarDefAnnot = No
  type MethodDefAnnot = No
  type TypeLitAnnot = No
  type BlockAnnot = No
  type ControlFlowStmtAnnot = No
  type ExprAnnot = No

}