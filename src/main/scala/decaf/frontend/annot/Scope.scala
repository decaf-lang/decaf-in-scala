package decaf.frontend.annot

import scala.collection.mutable

sealed trait Scope extends Annot {
  type Item <: Symbol
  type Owner <: Symbol

  protected var symbols: mutable.Map[String, Item] = new mutable.HashMap

  var owner: Owner = _

  def contains(key: String): Boolean = symbols.contains(key)

  def apply(key: String): Item = symbols(key)

  def lookup(key: String): Option[Item] = symbols.get(key)

  def declare(symbol: Item): Unit = symbols(symbol.name) = symbol

  override def toString: String = "{ " + symbols.map {
    case (name, symbol) => s"  $name -> $symbol"
  } mkString "\n" + " }"
}

object ScopedImplicit {

  implicit class __Scoped__[S <: Scope](self: Annotated[S]) {
    def scope: S = self.annot
  }

}

class GlobalScope extends Scope {
  type Item = ClassSymbol
}

class ClassScope(val parent: Option[ClassScope] = None) extends Scope {
  type Item = FieldSymbol
  type Owner = ClassSymbol
}

class FormalScope extends Scope {
  type Item = VarSymbol
  type Owner = FunSymbol
}

class LocalScope extends Scope {
  type Item = VarSymbol
}

class ScopeContext private(global: GlobalScope, private val scopes: List[Scope]) {

  def this(globalScope: GlobalScope) = this(globalScope, Nil)

  def open(scope: Scope): ScopeContext = scope match {
    case s: ClassScope => s.parent match {
      case Some(ps) => new ScopeContext(global, scope :: open(ps).scopes)
      case None => new ScopeContext(global, scope :: scopes)
    }
    case _ => new ScopeContext(global, scope :: scopes)
  }

  def currentClass: ClassSymbol = scopes.find(_.isInstanceOf[ClassScope]) match {
    case Some(scope) => scope.asInstanceOf[ClassScope].owner
    case None => throw new NoSuchElementException("current class symbol")
  }

  def currentFun: FunSymbol = scopes.find(_.isInstanceOf[FormalScope]) match {
    case Some(scope) => scope.asInstanceOf[FormalScope].owner
    case None => throw new NoSuchElementException("current function symbol")
  }

  /**
    * Lookup a symbol by key. By saying "lookup", the user expects that the symbol is found.
    * In this way, we will always search in all possible scopes and returns the innermost result.
    *
    * @param key
    * @return innermost found symbol (if any)
    */
  def lookup(key: String): Option[Symbol] = {
    def findIn(scopes: List[Scope]): Option[Symbol] = scopes match {
      case Nil => global.lookup(key)
      case scope :: ss => scope.lookup(key) match {
        case Some(symbol) => Some(symbol)
        case None => findIn(ss)
      }
    }

    findIn(scopes)
  }

  def apply(key: String): Symbol = lookup(key).get

  def lookupClass(key: String): Option[ClassSymbol] = global.lookup(key)

  def getClass(key: String): ClassSymbol = global(key)

  /**
    * Search a symbol by key. By saying "search", the user expects that the symbol is _not_ found.
    * In this way, we will try to search as "shallow" as possible. In particular:
    * - if the innermost scope can hide, i.e. FormalScope & LocalScope, search the innermost and global scope;
    * - otherwise, we have to search all possible scopes, i.e. like `lookup`.
    *
    * @param key
    * @return innermost found symbol (if any)
    */
  def search(key: String): Option[Symbol] =
    if (scopes.isEmpty) global.lookup(key)
    else scopes.head match {
      case _: FormalScope | _: LocalScope => scopes.head.lookup(key) match {
        case Some(symbol) => Some(symbol)
        case None => global.lookup(key)
      }
      case _ => lookup(key)
    }

  def lookupLocally(key: String): Option[Symbol] = (if (scopes.isEmpty) global else scopes.head) lookup key

  def declare(symbol: Symbol): Unit = {
    assert(scopes.nonEmpty)
    val scope = scopes.head
    scope.declare(symbol.asInstanceOf[scope.Item])
  }
}
