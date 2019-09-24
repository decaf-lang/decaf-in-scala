package decaf.frontend.annot

import java.util.TreeSet

import decaf.frontend.parsing.Pos
import decaf.frontend.tree.SyntaxTree
import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode.Var
import decaf.lowlevel.tac.ClassInfo
import decaf.util.Conversions._

/**
  * Symbols.
  *
  * A symbol is created when a definition is identified and type-checked, indicating a class/variable/method is
  * resolved successfully. Symbols are used in two ways:
  *
  *   - stored in the symbol table of a scope
  *   - referred by other expressions/statements
  */
sealed trait Symbol extends Annot with Ordered[Symbol] {

  type Typ <: Type

  /** Name. */
  def name: String

  /** Type. */
  def typ: Typ

  /** Position. */
  def pos: Pos

  /** String representation. */
  def str: String

  /**
    * In which scope is this symbol defined?
    */
  var domain: Scope = _

  override def toString: String = s"(${ pos.line },${ pos.column }) -> " + str

  /** Symbols are compared by their positions. */
  override def compare(that: Symbol): Int = this.pos.compare(that.pos)
}

/**
  * Class symbol, representing a class definition.
  *
  * @param tree   a [[decaf.frontend.tree.SyntaxTree.ClassDef]]
  * @param typ    type
  * @param scope  associated class scope of this class
  * @param parent symbols of super class (if any)
  */
class ClassSymbol(tree: ClassDef, val typ: ClassType, val scope: ClassScope, val parent: Option[ClassSymbol] = None)
  extends Symbol {

  type Typ = ClassType

  override def name: String = tree.name

  override def pos: Pos = tree.pos

  override def str: String = s"class $name" + (if (parent.isDefined) s" : ${ parent.get.name }" else "")

  scope.owner = this

  /** Returns the [[ClassInfo]] of this class, used by [[decaf.frontend.tac.TacGen]] */
  def getInfo: ClassInfo = {
    val memberVariables = new TreeSet[String]
    val memberMethods = new TreeSet[String]
    val staticMethods = new TreeSet[String]

    scope.values.foreach {
      case v: MemberVarSymbol => memberVariables.add(v.name)
      case m: MethodSymbol =>
        if (m.isStatic) {
          staticMethods.add(m.name)
        } else {
          memberMethods.add(m.name)
        }
    }

    new ClassInfo(name, parent.map(_.name), memberVariables, memberMethods, staticMethods, name == "Main")
  }
}

/**
  * Field symbol, representing a class member.
  */
trait FieldSymbol extends Symbol {

  /** Which class owns this member? */
  def owner: ClassSymbol
}

/**
  * Variable symbol, representing a variable definition.
  */
trait VarSymbol extends Symbol

/**
  * Member variable symbol, representing a member variable definition.
  *
  * @param tree  a [[decaf.frontend.tree.TreeNode.Var]]
  * @param typ   type
  * @param owner owner, a class symbol
  */
class MemberVarSymbol(tree: Var, val typ: Type, val owner: ClassSymbol) extends FieldSymbol with VarSymbol {

  type Typ = Type

  override def name: String = tree.name

  override def pos: Pos = tree.pos

  override def str: String = s"variable $name : $typ"
}

/**
  * Method symbol, representing a method definition.
  *
  * @param tree  a [[decaf.frontend.tree.SyntaxTree.MethodDef]]
  * @param typ   type
  * @param scope associated formal scope of the method parameters
  * @param owner owner, a class symbol
  */
class MethodSymbol(tree: SyntaxTree.MethodDef, val typ: FunType, val scope: FormalScope, val owner: ClassSymbol)
  extends FieldSymbol {

  type Typ = FunType

  override def name: String = tree.name

  override def pos: Pos = tree.pos

  override def str: String =
    (if (tree.modifiers.isEmpty) "" else s"${ tree.modifiers } ") + s"function $name : " + typ.toString

  scope.owner = this

  val arity: Int = typ.args.length

  val returnType: Type = typ.ret

  val isStatic: Boolean = tree.isStatic

  private var main = false

  /** Is this method the main method? */
  def isMain: Boolean = main

  def setMain(): Unit = main = true
}

/**
  * Local variable symbol, representing a local variable definition.
  *
  * @param name name
  * @param typ  type
  * @param pos  position
  */
class LocalVarSymbol(val name: String, val typ: Type, val pos: Pos) extends Symbol with VarSymbol {

  type Typ = Type

  /** Create a local variable symbol by a [[decaf.frontend.tree.TreeNode.Var]] and its type. */
  def this(tree: Var, typ: Type) = this(tree.name, typ, tree.pos)

  /** Is this a parameter? */
  def isParam: Boolean = domain.isFormal

  override def str: String = s"variable " + (if (isParam) "@" else "") + s"$name : $typ"
}

object LocalVarSymbol {

  /** Create a special "this" symbol its type and position. */
  def thisVar(typ: Type, pos: Pos): LocalVarSymbol = new LocalVarSymbol("this", typ, pos)
}

object SymbolImplicit {

  implicit class SymbolAnnotatedHasSymbol[S <: Symbol](self: Annotated[S]) {

    /**
      * Access a node that is annotated with a [[Symbol]] by the field name `symbol`.
      *
      * @example If `x` is annotated with a [[ClassSymbol]], then {{{ x.symbol }}} gives you {{{ x.annot: ClassSymbol
      * }}}.
      *
      * @return the annotation
      */
    def symbol: S = self.annot
  }

}