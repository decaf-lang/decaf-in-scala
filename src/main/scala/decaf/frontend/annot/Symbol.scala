package decaf.frontend.annot

import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode.{Def, VarDecl}

import scala.util.parsing.input.Positional

trait Symbol extends Annot with Positional {
  type DefT <: Def
  protected val tree: DefT

  type TypeT <: Type
  val typ: TypeT

  val name: String = tree.name
  pos = tree.pos
}

object SymbolizedImplicit {

  implicit class Symbolized[S <: Symbol](self: Annotated[S]) {
    def symbol: S = self.annot
  }

}

class ClassSymbol(override protected val tree: ClassDef, override val typ: ClassType,
                  val scope: ClassScope) extends Symbol {
  type DefT = ClassDef
  type TypeT = ClassType

  def lookup(key: String): Option[FieldSymbol] =
    new ScopeContext(new GlobalScope).open(scope).lookup(key).map(_.asInstanceOf[FieldSymbol])

  override def toString: String = s"!Class!$name!$typ"

  scope.owner = this
}

trait FieldSymbol extends Symbol

class VarSymbol(override protected val tree: VarDecl, override val typ: Type, val kind: VarKind = OtherVar)
  extends FieldSymbol {
  type DefT = VarDecl
  type TypeT = Type

  override def toString: String = s"!Var!$name!$typ"
}

// TODO see how to do it elegantly in future
sealed trait VarKind

object MemberVar extends VarKind

object OtherVar extends VarKind

class FunSymbol(override protected val tree: MethodDef, override val typ: FunType,
                paramSymbols: List[VarSymbol], val scope: FormalScope, val parent: ClassSymbol)
  extends FieldSymbol {

  type DefT = MethodDef
  type TypeT = FunType

  val arity: Int = paramSymbols.length

  val returnType: Type = typ.ret

  def isStatic: Boolean = tree.isStatic

  def isMain: Boolean = tree.isStatic && (typ eq FunType(Nil, VoidType))

  override def toString: String = s"!Fun!$name!$typ"

  scope.owner = this
}
