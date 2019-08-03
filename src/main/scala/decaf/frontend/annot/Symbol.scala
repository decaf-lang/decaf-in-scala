package decaf.frontend.annot

import decaf.frontend.tree.SyntaxTree._

import scala.util.parsing.input.Positional

trait Symbol extends Positional {
  type DefT <: Def
  protected val tree: DefT

  type TypeT <: Type
  val typ: TypeT

  val name: String = tree.name
  pos = tree.pos

  var definedIn: Scope = _
}

class VarSymbol(override protected val tree: VarDef, override val typ: Type) extends Symbol {
  type DefT = VarDef
  type TypeT = Type

  override def toString: String = s"!Var!$name!$typ"
}

class FunSymbol(override protected val tree: MethodDef, override val typ: FunType) extends Symbol {
  type DefT = MethodDef
  type TypeT = FunType

  override def toString: String = s"!Fun!$name!$typ"
}

class ClassSymbol(override protected val tree: ClassDef, override val typ: ClassType) extends Symbol {
  type DefT = ClassDef
  type TypeT = ClassType

  def scope: ClassScope = ???

  override def toString: String = s"!Class!$name!$typ"
}
