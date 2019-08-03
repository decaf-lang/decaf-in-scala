package decaf.frontend.error

import scala.util.parsing.input.{NoPosition, Position}

abstract class Error(msg: String, pos: Position = NoPosition) {
  override def toString: String = pos match {
    case NoPosition => s"*** Error: $msg"
    case _ => s"*** Error at (${pos.line},${pos.column}): $msg"
  }
}

// Resolvers

class BadArrElementError(pos: Position)
  extends Error("array element type must be non-void type", pos)

class BadInheritanceError(pos: Position)
  extends Error("illegal class inheritance (should be acyclic)", pos)

class BadOverrideError(fun: String, parent: String, pos: Position)
  extends Error(s"overriding method '$fun' doesn't match the type signature in class '$parent'", pos)

class BadVarTypeError(id: String, pos: Position)
  extends Error(s"cannot declare identifier '$id' as void type")

class ClassNotFoundError(id: String, pos: Position)
  extends Error(s"class '$id' not found")

class DeclConflictError(id: String, earlier: Position, pos: Position)
  extends Error(s"declaration of '$id' here conflicts with earlier declaration at " +
    s"(${earlier.line},${earlier.column})")

object NoMainClassError extends Error("no legal Main class named 'Main' was found")

class OverridingVarError(id: String, pos: Position)
  extends Error(s"overriding variable is not allowed for var '$id'", pos)
