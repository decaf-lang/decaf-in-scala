package decaf.frontend.annot

import decaf.frontend.tree.SyntaxTree._

import scala.util.parsing.input.Positional

trait Symbol extends Annot with Positional {
  type DefT <: Def
  protected val tree: DefT

  type TypeT <: Type
  val typ: TypeT

  val name: String = tree.name
  pos = tree.pos
}


class ClassSymbol(override protected val tree: ClassDef, override val typ: ClassType,
                  val scope: ClassScope) extends Symbol {
  type DefT = ClassDef
  type TypeT = ClassType

  override def toString: String = s"!Class!$name!$typ"
}

trait FieldSymbol extends Symbol

class VarSymbol(override protected val tree: VarDef, override val typ: Type) extends FieldSymbol {
  type DefT = VarDef
  type TypeT = Type

  override def toString: String = s"!Var!$name!$typ"
}

class FunSymbol(override protected val tree: MethodDef, override val typ: FunType,
                paramSymbols: List[VarSymbol], scope: FormalScope, val parent: ClassSymbol)
  extends FieldSymbol {
  type DefT = MethodDef
  type TypeT = FunType

  def isStatic: Boolean = tree.isStatic

  def isMain: Boolean = tree.isStatic && (typ eq FunType(Nil, VoidType))

  override def toString: String = s"!Fun!$name!$typ"
}


object ResolvedImplicit {

  implicit class Resolved[S <: Symbol](self: Annotated[S]) {
    def symbol: S = self.annot
  }

}