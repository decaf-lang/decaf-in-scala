package decaf.annot

import decaf.tree.SyntaxTree._
import decaf.tree.TreeNode.{Def, Id, Var}
import decaf.parsing.{Pos, Positional}

sealed trait Symbol extends Annot with Positional {
  type DefT <: Def
  protected val tree: DefT

  type TypeT <: Type
  val typ: TypeT

  val name: String = tree.name
  pos = tree.pos

  var definedIn: Scope = _

  def str: String

  override def toString: String = s"(${ pos.line },${ pos.column }) -> " + str
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

  scope.owner = this

  override def str: String = s"class $name" + (if (parent.isDefined) s" : ${ parent.get.name }" else "")
}

trait FieldSymbol extends Symbol {
  val owner: ClassSymbol
}

trait VarSymbol extends Symbol

class MemberVarSymbol(override protected val tree: Var, override val typ: Type, val owner: ClassSymbol)
  extends FieldSymbol with VarSymbol {
  type DefT = Var
  type TypeT = Type

  override def str: String = s"variable $name : $typ"
}

class MethodSymbol(override protected val tree: MethodDef, override val typ: FunType,
                   paramSymbols: List[LocalVarSymbol], val scope: FormalScope, val owner: ClassSymbol,
                   val overrides: Option[MethodSymbol] = None)
  extends FieldSymbol {

  type DefT = MethodDef
  type TypeT = FunType

  val arity: Int = paramSymbols.length

  val returnType: Type = typ.ret

  def isStatic: Boolean = tree.isStatic

  def isMain: Boolean = _isMain

  private var _isMain = false

  def setMain(): Unit = _isMain = true

  scope.owner = this

  override def str: String = (if (isStatic) "static " else "") + s"function $name : " +
    (if (isStatic) "" else s"class : ${ owner.name }->") + typ.toString
}

class LocalVarSymbol(override val tree: Var, override val typ: Type, val isParam: Boolean = false)
  extends Symbol with VarSymbol {
  type DefT = Var
  type TypeT = Type

  def this(typ: Type, pos: Pos) = this(LocalVarDef(TVoid(), Id("this")).setPos(pos), typ, true)

  override def str: String = s"variable " + (if (isParam) "@" else "") + s"$name : $typ"
}