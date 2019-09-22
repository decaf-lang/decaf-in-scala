package decaf.frontend.annot

import decaf.frontend.parsing.Pos

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

  def isEmpty: Boolean = symbols.isEmpty

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
    symbol.domain = this
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

/**
  * A symbol table, which is organized as a stack of scopes, maintained by {@link decaf.frontend.typecheck.Namer}.
  * A typical full scope stack looks like the following:
  * {{{
  *     LocalScope   --- stack top (current scope)
  *     ...          --- many nested local scopes
  *     LocalScope
  *     FormalScope
  *     ClassScope
  *     ...          --- many parent class scopes
  *     ClassScope
  *     GlobalScope  --- stack bottom
  * }}}
  * Make sure the global scope is always at the bottom, and NO class scope appears in neither formal nor local scope.
  *
  * @see Scope
  */
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
    * Find a symbol by key. Search in all possible scopes while the predicate `cond` holds, and the found symbols
    * satisfies `p`. Returns the innermost result.
    *
    * @param key    the key
    * @param cond   the predicate over the currently seeing scope
    * @param p      the predicate over the suspect symbol
    * @param scopes all remaining scopes to be searched
    * @return innermost found symbol (if any)
    */
  @scala.annotation.tailrec
  private def findWhile(key: String, cond: Scope => Boolean = _ => true, p: Symbol => Boolean = _ => true,
                        scopes: List[Scope] = scopes :+ global): Option[Symbol] =
    scopes match {
      case Nil => None
      case s :: ss =>
        if (!cond(s)) None
        else s.find(key) match {
          case Some(symbol) if p(symbol) => Some(symbol)
          case _ => findWhile(key, cond, p, ss)
        }
    }

  /**
    * Lookup a symbol by key. By saying "lookup", the user expects that the symbol is found.
    * In this way, we will always search in all possible scopes and returns the innermost result.
    *
    * @param key symbol's name
    * @return innermost found symbol (if any)
    */
  def lookup(key: String): Option[Symbol] = findWhile(key)

  /**
    * Same with {@link #lookup} but we restrict the symbol's position to be before the given `pos`.
    *
    * @param key symbol's name
    * @param pos position
    * @return innermost found symbol before `pos` (if any)
    */
  def lookupBefore(key: String, pos: Pos): Option[Symbol] = findWhile(key, _ => true,
    s => !(s.domain.isLocalOrFormal && s.pos >= pos))

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