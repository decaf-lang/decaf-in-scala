package decaf.frontend.annot

import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode.{Def, Var}

import scala.util.parsing.input.Positional

sealed trait Symbol extends Annot with Positional {
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

  implicit def __getSymbolName__(sym: Symbol): String = sym.name

}

class ClassSymbol(override protected val tree: ClassDef, override val typ: ClassType,
                  val scope: ClassScope, val parent: Option[ClassSymbol] = None) extends Symbol {
  type DefT = ClassDef
  type TypeT = ClassType

  def vars: List[MemberVarSymbol] = scope.collect {
    // TODO in future use isMemberVar
    case s: MemberVarSymbol => Some(s)
    case _ => None
  }

  def methods: List[MethodSymbol] = scope.collect {
    case s: MethodSymbol => Some(s)
    case _ => None
  }

  def memberMethods: List[MethodSymbol] = methods.filterNot(_.isStatic)

  def lookup(key: String): Option[FieldSymbol] =
    new ScopeContext(new GlobalScope).open(scope).lookup(key).map(_.asInstanceOf[FieldSymbol])

  override def toString: String = s"!Class!$name!$typ"

  scope.owner = this
}

trait FieldSymbol extends Symbol

trait VarSymbol extends Symbol

class MemberVarSymbol(override protected val tree: Var, override val typ: Type)
  extends FieldSymbol with VarSymbol {
  type DefT = Var
  type TypeT = Type

  override def toString: String = s"!Var!$name!$typ"
}

class MethodSymbol(override protected val tree: MethodDef, override val typ: FunType,
                   paramSymbols: List[LocalVarSymbol], val scope: FormalScope, val parent: ClassSymbol)
  extends FieldSymbol {

  type DefT = MethodDef
  type TypeT = FunType

  val arity: Int = paramSymbols.length

  val returnType: Type = typ.ret

  def isStatic: Boolean = tree.isStatic

  def isMainSig: Boolean = tree.isStatic && (typ eq FunType(Nil, VoidType))

  override def toString: String = s"!Fun!$name!$typ"

  scope.owner = this
}

class LocalVarSymbol(override val tree: Var, override val typ: Type) extends Symbol with VarSymbol {
  type DefT = Var
  type TypeT = Type

  override def toString: String = s"!Var!$name!$typ"
}