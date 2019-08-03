package decaf.frontend.annot

import decaf.frontend.tree.SyntaxTree.Block

import scala.collection.mutable


class Scope {
  protected var symbols: mutable.Map[String, Symbol] = new mutable.HashMap

  def lookup(key: String): Option[Symbol] = symbols.get(key)

  def declare(symbol: Symbol): Unit = {
    symbols(symbol.name) = symbol
    symbol.definedIn = this
  }

  override def toString: String = "{ " + symbols.map {
    case (name, symbol) => s"  $name -> $symbol"
  } mkString "\n" + " }"
}

object NoScope extends Scope

class LocalScope(block: Block) extends Scope

class FormalScope(block: Block, owner: FunSymbol) extends Scope

class ClassScope(val owner: ClassSymbol) extends Scope

class GlobalScope extends Scope

class ScopeContext private(scopes: List[Scope] = Nil) {
  def ::(scope: Scope): ScopeContext = scope match {
    case _: GlobalScope => new ScopeContext(List(scope))
    case s: ClassScope =>
      def go(scope: ClassScope, acc: List[Scope]): List[Scope] = scope.owner.typ.parent match {
        case Some(ClassType(name, _)) =>
          val scope1 = this (name).asInstanceOf[ClassScope]
          go(scope1, acc :+ scope1)
        case None => acc
      }

      new ScopeContext(go(s, List(s)) ++ scopes)
    case _ => new ScopeContext(scope :: scopes)
  }

  /**
    * Lookup a symbol by key. Only search the innermost scope.
    *
    * @param key
    * @return
    */
  def lookup(key: String): Option[Symbol] = {
    assert(scopes.nonEmpty)
    scopes.head.lookup(key)
  }

  def lookupClass(key: String): Option[ClassSymbol] = {
    assert(scopes.nonEmpty)
    scopes.head.lookup(key) match {
      case Some(symbol: ClassSymbol) => Some(symbol)
      case _ => None
    }
  }

  /**
    * Lookup a symbol by key. Search every possible scope.
    *
    * @param key
    * @return
    */
  def lookupGlobally(key: String): Option[Symbol] = {
    def lookupIn(scopes: List[Scope]): Option[Symbol] = scopes.head.lookup(key) match {
      case Some(symbol) => Some(symbol)
      case None => if (scopes.tail.nonEmpty) lookupIn(scopes.tail) else None
    }

    lookupIn(scopes)
  }

  def apply(key: String): Symbol = lookupGlobally(key).get

  def declare(symbol: Symbol): Unit = {
    assert(scopes.nonEmpty)
    scopes.head.declare(symbol)
  }
}

object ScopeContext {
  def empty: ScopeContext = new ScopeContext
}

