package decaf.error

import decaf.frontend.annot.Type
import decaf.frontend.tree.TreeNode.{BinaryOp, UnaryOp}

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

class BadVarTypeError(id: String, pos: Position)
  extends Error(s"cannot declare identifier '$id' as void type")

class BadInheritanceError(pos: Position)
  extends Error("illegal class inheritance (should be acyclic)", pos)

class BadOverrideError(fun: String, parent: String, pos: Position)
  extends Error(s"overriding method '$fun' doesn't match the type signature in class '$parent'", pos)

class OverridingVarError(id: String, pos: Position)
  extends Error(s"overriding variable is not allowed for var '$id'", pos)

class ClassNotFoundError(clazz: String, pos: Position)
  extends Error(s"class '$clazz' not found")

class DeclConflictError(id: String, earlier: Position, pos: Position)
  extends Error(s"declaration of '$id' here conflicts with earlier declaration at " +
    s"(${earlier.line},${earlier.column})")

object NoMainClassError extends Error("no legal Main class named 'Main' was found")

// Typer

class BreakOutOfLoopError(pos: Position)
  extends Error("'break' is only allowed inside a loop")

class BadTestExpr(pos: Position)
  extends Error("test expression must have bool type")

class BadPrintArgError(k: Int, actual: Type, pos: Position)
  extends Error(s"incompatible argument $k: $actual given, int/bool/string expected")

class BadReturnTypeError(expected: Type, actual: Type, pos: Position)
  extends Error(s"incompatible return: $actual given, $expected expected")

class IncompatUnOpError(op: UnaryOp, exprType: Type, pos: Position)
  extends Error(s"incompatible operand: $op $exprType")

class IncompatBinOpError(op: BinaryOp, lhsType: Type, rhsType: Type, pos: Position)
  extends Error(s"incompatible operands: $lhsType $op $rhsType")

class NotArrayError(pos: Position)
  extends Error("[] can only be applied to arrays")

class SubNotIntError(pos: Position)
  extends Error("array subscript must be an integer")

class BadNewArrayLength(pos: Position)
  extends Error("new array length must be an integer")

class ThisInStaticFuncError(pos: Position)
  extends Error("can not use this in static function")

class UndeclVarError(v: String, pos: Position)
  extends Error(s"undeclared variable '$v'")

class NotClassError(typ: Type, pos: Position)
  extends Error(s"$typ is not a class type")

class FieldNotFoundError(field: String, clazz: String, pos: Position)
  extends Error(s"field '$field' not found in '$clazz'")

class FieldNotAccessError(field: String, clazz: String, pos: Position)
  extends Error(s"field '$field' of '$clazz' not accessible here")

class RefNonStaticError(field: String, method: String, pos: Position)
  extends Error(s"can not reference a non-static field '$field' from static method from '$method'")

class NotClassFieldError(field: String, typ: String, pos: Position)
  extends Error(s"cannot access field '$field' from '$typ'")

class NotClassMethodError(field: String, clazz: String, pos: Position)
  extends Error(s"'$field' is not a method in class '$clazz'")

class BadArgCountError(method: String, expected: Int, actual: Int, pos: Position)
  extends Error(s"function '$method' expects $expected argument(s) but $actual given")

class BadArgTypeError(k: Int, expected: String, actual: String, pos: Position)
  extends Error(s"incompatible argument $k: $actual given, $expected expected")

class BadLengthArgError(count: Int, pos: Position)
  extends Error(s"function 'length' expects 0 argument(s) but $count given")

