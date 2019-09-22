package decaf.frontend.annot

import java.util.TreeSet

import decaf.lowlevel.tac.ClassInfo
import decaf.frontend.parsing.Pos
import decaf.frontend.tree.SyntaxTree
import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode.Var
import decaf.util.Conversions._

/**
  * Symbols.
  *
  * A symbol is created when a definition is identified and type-checked, indicating a class/variable/method is
  * resolved successfully. Symbols are used in two ways:
  * - stored in the symbol table of a scope
  * - referred by other expressions/statements
  */
sealed trait Symbol extends Annot with Ordered[Symbol] {
  type Typ <: Type

  def name: String

  def typ: Typ

  def pos: Pos

  def str: String

  var domain: Scope = _

  override def toString: String = s"(${ pos.line },${ pos.column }) -> " + str

  override def compare(that: Symbol): Int = this.pos.compare(that.pos)
}

class ClassSymbol(tree: ClassDef, val typ: ClassType, val scope: ClassScope, val parent: Option[ClassSymbol] = None)
  extends Symbol {
  type Typ = ClassType

  override def name: String = tree.name

  override def pos: Pos = tree.pos

  override def str: String = s"class $name" + (if (parent.isDefined) s" : ${ parent.get.name }" else "")

  scope.owner = this

  def vars: List[MemberVarSymbol] = scope.flatMap {
    case s: MemberVarSymbol => Some(s)
    case _ => None
  }

  def methods: List[MethodSymbol] = scope.flatMap {
    case s: MethodSymbol => Some(s)
    case _ => None
  }

  def memberMethods: List[MethodSymbol] = methods.filterNot(_.isStatic)

  def getInfo: ClassInfo = {
    val memberVariables = new TreeSet[String]
    val memberMethods = new TreeSet[String]
    val staticMethods = new TreeSet[String]

    scope.values.foreach {
      case v: MemberVarSymbol => memberVariables.add(v.name)
      case m: MethodSymbol =>
        if (m.isStatic) staticMethods.add(m.name)
        else memberMethods.add(m.name)
    }

    new ClassInfo(name, parent.map(_.name), memberVariables, memberMethods, staticMethods, name == "Main")
  }
}

trait FieldSymbol extends Symbol {
  def owner: ClassSymbol
}

trait VarSymbol extends Symbol

class MemberVarSymbol(tree: Var, val typ: Type, val owner: ClassSymbol) extends FieldSymbol with VarSymbol {
  type Typ = Type

  override def name: String = tree.name

  override def pos: Pos = tree.pos

  override def str: String = s"variable $name : $typ"
}

class MethodSymbol(tree: SyntaxTree.MethodDef, val typ: FunType, val scope: FormalScope, val owner: ClassSymbol,
                   val overrides: Option[MethodSymbol] = None) extends FieldSymbol {
  type Typ = FunType

  override def name: String = tree.name

  override def pos: Pos = tree.pos

  override def str: String =
    (if (tree.modifiers.isEmpty) "" else s"${ tree.modifiers } ") + s"function $name : " + typ.toString

  scope.owner = this

  val arity: Int = typ.params.length

  val returnType: Type = typ.ret

  val isStatic: Boolean = tree.isStatic

  private var main = false

  def isMain: Boolean = main

  def setMain(): Unit = main = true
}

class LocalVarSymbol(val name: String, val typ: Type, val pos: Pos, isParam: Boolean) extends Symbol with VarSymbol {
  type Typ = Type

  def this(tree: Var, typ: Type, isParam: Boolean = false) = this(tree.name, typ, tree.pos, isParam)

  override def str: String = s"variable " + (if (isParam) "@" else "") + s"$name : $typ"
}

object LocalVarSymbol {
  def thisVar(typ: Type, pos: Pos): LocalVarSymbol = new LocalVarSymbol("this", typ, pos, true)
}

object SymbolImplicit {

  implicit class SymbolAnnotatedHasSymbol[S <: Symbol](self: Annotated[S]) {
    def symbol: S = self.annot
  }

}