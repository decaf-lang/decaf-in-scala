package decaf.tree

import decaf.annot.Annot
import decaf.tree.TreeNode.Id

object SyntaxTree extends TreeTmpl {

  implicit object NoAnnot extends Annot {
    override def toString: String = ""
  }

  type No = NoAnnot.type

  type TopLevelAnnot = No
  type ClassAnnot = No
  type MemberVarAnnot = No
  type LocalVarAnnot = No
  type MethodAnnot = No
  type TypeLitAnnot = No
  type StmtAnnot = No
  type ExprAnnot = No

  type ClassRef = Id

  // The following nodes only appear in a syntax tree.

  /**
    * Field selection:
    * {{{
    *   [<receiver>.]<field>
    * }}}
    *
    * @param receiver target instance
    * @param variable identifier of the selected variable
    */
  case class VarSel(receiver: Option[Expr], variable: Id)(implicit val annot: ExprAnnot) extends LValue {
    def withReceiver(receiver: Expr): VarSel = VarSel(Some(receiver), variable)(annot).setPos(pos)
  }

  /**
    * Call expression:
    * {{{
    *   [<receiver>.]<id>(<arg1>, <arg2>, ...)
    * }}}
    *
    * @param receiver target instance, `this` if not given
    * @param method   identifier of the selected method
    * @param args     arguments
    */
  case class Call(receiver: Option[Expr], method: Id, args: List[Expr])(implicit val annot: ExprAnnot) extends Expr {
    def withReceiver(receiver: Expr): Call = Call(Some(receiver), method, args)(annot).setPos(pos)

    def asExprEval: ExprEval = ExprEval(this)(annot).setPos(pos)
  }

}