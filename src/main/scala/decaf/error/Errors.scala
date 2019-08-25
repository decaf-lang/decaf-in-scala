package decaf.error

import decaf.frontend.annot.Type
import decaf.frontend.tree.TreeNode.Op

import decaf.parsing.{Position, NoPosition}

abstract class Error(val msg: String, val pos: Position = NoPosition) {
  override def toString: String = pos match {
    case NoPosition => s"*** Error: $msg"
    case _ => s"*** Error at (${ pos.line },${ pos.column }): $msg"
  }
}

class SyntaxError(pos: Position)
  extends Error("syntax error", pos)

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
  extends Error(s"class '$clazz' not found", pos)

class DeclConflictError(id: String, earlier: Position, pos: Position)
  extends Error(s"declaration of '$id' here conflicts with earlier declaration at " +
    s"(${ earlier.line },${ earlier.column })", pos)

object NoMainClassError extends Error("no legal Main class named 'Main' was found")

// Typer

class BreakOutOfLoopError(pos: Position)
  extends Error("'break' is only allowed inside a loop", pos)

class BadTestExpr(pos: Position)
  extends Error("test expression must have bool type", pos)

class BadPrintArgError(k: Int, actual: Type, pos: Position)
  extends Error(s"incompatible argument $k: $actual given, int/bool/string expected", pos)

class BadReturnTypeError(expected: Type, actual: Type, pos: Position)
  extends Error(s"incompatible return: $actual given, $expected expected", pos)

class IncompatUnOpError(op: Op, exprType: Type, pos: Position)
  extends Error(s"incompatible operand: $op $exprType", pos)

class IncompatBinOpError(op: Op, lhsType: Type, rhsType: Type, pos: Position)
  extends Error(s"incompatible operands: $lhsType $op $rhsType", pos)

class NotArrayError(pos: Position)
  extends Error("[] can only be applied to arrays", pos)

class SubNotIntError(pos: Position)
  extends Error("array subscript must be an integer", pos)

class BadNewArrayLength(pos: Position)
  extends Error("new array length must be an integer", pos)

class ThisInStaticFuncError(pos: Position)
  extends Error("can not use this in static function", pos)

class UndeclVarError(v: String, pos: Position)
  extends Error(s"undeclared variable '$v'", pos)

class NotClassError(typ: Type, pos: Position)
  extends Error(s"$typ is not a class type", pos)

class FieldNotFoundError(field: String, clazz: String, pos: Position)
  extends Error(s"field '$field' not found in '$clazz'", pos)

class FieldNotAccessError(field: String, clazz: String, pos: Position)
  extends Error(s"field '$field' of '$clazz' not accessible here", pos)

class RefNonStaticError(field: String, method: String, pos: Position)
  extends Error(s"can not reference a non-static field '$field' from static method from '$method'", pos)

class NotClassFieldError(field: String, typ: String, pos: Position)
  extends Error(s"cannot access field '$field' from '$typ'", pos)

class NotClassMethodError(field: String, clazz: String, pos: Position)
  extends Error(s"'$field' is not a method in class '$clazz'", pos)

class BadArgCountError(method: String, expected: Int, actual: Int, pos: Position)
  extends Error(s"function '$method' expects $expected argument(s) but $actual given", pos)

class BadArgTypeError(k: Int, expected: String, actual: String, pos: Position)
  extends Error(s"incompatible argument $k: $actual given, $expected expected", pos)

class BadLengthArgError(count: Int, pos: Position)
  extends Error(s"function 'length' expects 0 argument(s) but $count given", pos)
