package decaf.annot

import scala.collection.mutable

/**
  * Scopes.
  *
  * A scope stores the mapping of symbol names to symbols. Every scope has an owner (a symbol).
  */
sealed trait Scope extends Annot {
  type Item <: Symbol
  type Owner <: Symbol

  protected var symbols: mutable.Map[String, Item] = new mutable.HashMap

  var owner: Owner = _

  def values: List[Item] = symbols.values.toList.sortBy(_.pos)

  def flatMap[T <: Item](p: Item => Option[T]): List[T] = symbols.values.flatMap(p).toList

  def contains(key: String): Boolean = symbols.contains(key)

  def apply(key: String): Item = symbols(key)

  /**
    * Find a `key` in the current symbol table only.
    *
    * @param key the key
    * @return the matched symbol (if any)
    */
  def find(key: String): Option[Item] = symbols.get(key)

  def declare(symbol: Item): Unit = {
    symbols(symbol.name) = symbol
  }

  def isLocalOrFormal: Boolean = false

  override def toString: String = "{ " + symbols.map {
    case (name, symbol) => s"  $name -> $symbol"
  } mkString "\n" + " }"
}

class GlobalScope extends Scope {
  type Item = ClassSymbol
}

class ClassScope(val parent: Option[ClassScope] = None) extends Scope {
  type Item = FieldSymbol
  type Owner = ClassSymbol

  /**
    * Lookup a symbol by key. Search in all parent and ancestor scopes, returns the innermost result.
    *
    * @param key the key
    * @return innermost found symbol (if any)
    */
  def lookup(key: String): Option[FieldSymbol] = find(key).orElse(parent.flatMap(_.lookup(key)))
}

class FormalScope extends Scope {
  type Item = LocalVarSymbol
  type Owner = MethodSymbol

  override def isLocalOrFormal: Boolean = true

  val nestedScope: LocalScope = new LocalScope
}

class LocalScope extends Scope {
  type Item = LocalVarSymbol

  override def isLocalOrFormal: Boolean = true

  val nestedScopes: mutable.ArrayBuffer[LocalScope] = new mutable.ArrayBuffer[LocalScope]
}

class ScopeContext private(global: GlobalScope, private val scopes: List[Scope], val currentScope: Scope,
                           val currentClass: ClassSymbol, val currentMethod: MethodSymbol) {

  def this(globalScope: GlobalScope) = this(globalScope, Nil, globalScope, null, null)

  def open(scope: Scope): ScopeContext = scope match {
    case s: ClassScope => s.parent match {
      case Some(ps) => new ScopeContext(global, s :: open(ps).scopes, s, s.owner, null)
      case None => new ScopeContext(global, s :: scopes, s, s.owner, null)
    }
    case s: FormalScope => new ScopeContext(global, s :: scopes, s, currentClass, s.owner)
    case s: LocalScope => new ScopeContext(global, s :: scopes, s, currentClass, currentMethod)
  }

  /**
    * Find a symbol by key. Search in all possible scopes while the predicate `p` holds, and returns the innermost
    * result.
    *
    * @param key    the key
    * @param p      the predicate over the currently seeing scope
    * @param scopes all remaining scopes to be searched
    * @return innermost found symbol (if any)
    */
  @scala.annotation.tailrec
  private def findWhile(key: String, p: Scope => Boolean, scopes: List[Scope] = scopes :+ global): Option[Symbol] =
    scopes match {
      case Nil => if (!p(global)) None else global.find(key)
      case s :: ss =>
        if (!p(s)) None
        else s.find(key) match {
          case Some(symbol) => Some(symbol)
          case None => findWhile(key, p, ss)
        }
    }

  /**
    * Lookup a symbol by key. By saying "lookup", the user expects that the symbol is found.
    * In this way, we will always search in all possible scopes and returns the innermost result.
    *
    * @param key the key
    * @return innermost found symbol (if any)
    */
  def lookup(key: String): Option[Symbol] = findWhile(key, _ => true)

  /**
    * Find if `key` conflicts with some already defined symbol. Rules:
    * - if the current scope is local scope or formal scope, `key` cannot conflict with any already defined symbol
    * up till the formal scope, and it cannot conflict with any names in the global scope
    * - if the current scope is class scope or global scope, `key` cannot conflict with any already defined symbol
    *
    * NO override checking is performed here, the type checker should tell if the returned conflicting symbol is
    * in fact allowed or not.
    *
    * @param key the key
    * @return innermost conflicting symbol (if any)
    */
  def findConflict(key: String): Option[Symbol] = currentScope match {
    case s if s.isLocalOrFormal => findWhile(key, _.isLocalOrFormal).orElse(global.find(key))
    case _ => lookup(key)
  }

  def apply(key: String): Symbol = lookup(key).get

  def containsClass(key: String): Boolean = global.contains(key)

  def lookupClass(key: String): Option[ClassSymbol] = global.find(key)

  def getClass(key: String): ClassSymbol = global(key)

  def declare(symbol: Symbol): Unit = currentScope.declare(symbol.asInstanceOf[currentScope.Item])
}

object ScopeImplicit {

  implicit class ScopeAnnotatedHasScope[S <: Scope](self: Annotated[S]) {
    def scope: S = self.annot
  }

}