package decaf.error

import decaf.annot.{ClassType, Type}
import decaf.parsing.{NoPos, Pos}

abstract class Error(val msg: String, val pos: Pos = NoPos) extends Exception {
  override def toString: String = pos match {
    case NoPos => s"*** Error: $msg"
    case _ => s"*** Error at (${ pos.line },${ pos.column }): $msg"
  }
}

// Lexer errors

class NewlineInStrError(literal: String, pos: Pos)
  extends Error(s"illegal newline in string constant $literal", pos)

class UntermStrError(literal: String, pos: Pos)
  extends Error(s"unterminated string constant $literal", pos)

class UnrecogCharError(char: Char, pos: Pos)
  extends Error(s"unrecognized character '$char'", pos)

class IntTooLargeError(literal: String, pos: Pos)
  extends Error(s"integer literal $literal is too large", pos)

// Syntax error

class SyntaxError(msg: String, pos: Pos)
  extends Error("syntax error", pos)

// Namer errors

class BadArrElementError(pos: Pos)
  extends Error("array element type must be non-void known type", pos)

class BadVarTypeError(id: String, pos: Pos)
  extends Error(s"cannot declare identifier '$id' as void type", pos)

class BadInheritanceError(pos: Pos)
  extends Error("illegal class inheritance (should be acyclic)", pos)

class BadOverrideError(fun: String, parent: String, pos: Pos)
  extends Error(s"overriding method '$fun' doesn't match the type signature in class '$parent'", pos)

class OverridingVarError(id: String, pos: Pos)
  extends Error(s"overriding variable is not allowed for var '$id'", pos)

class ClassNotFoundError(clazz: String, pos: Pos)
  extends Error(s"class '$clazz' not found", pos)

class DeclConflictError(id: String, earlier: Pos, pos: Pos)
  extends Error(s"declaration of '$id' here conflicts with earlier declaration at " +
    s"(${ earlier.line },${ earlier.column })", pos)

object NoMainClassError extends Error("no legal Main class named 'Main' was found")

// Typer errors

class BreakOutOfLoopError(pos: Pos)
  extends Error("'break' is only allowed inside a loop", pos)

class BadTestExpr(pos: Pos)
  extends Error("test expression must have bool type", pos)

class BadPrintArgError(k: Int, actual: Type, pos: Pos)
  extends Error(s"incompatible argument $k: $actual given, int/bool/string expected", pos)

class BadReturnTypeError(expected: Type, actual: Type, pos: Pos)
  extends Error(s"incompatible return: $actual given, $expected expected", pos)

class IncompatUnOpError(op: String, exprType: Type, pos: Pos)
  extends Error(s"incompatible operand: $op $exprType", pos)

class IncompatBinOpError(op: String, lhsType: Type, rhsType: Type, pos: Pos)
  extends Error(s"incompatible operands: $lhsType $op $rhsType", pos)

class NotArrayError(pos: Pos)
  extends Error("[] can only be applied to arrays", pos)

class SubNotIntError(pos: Pos)
  extends Error("array subscript must be an integer", pos)

class BadNewArrayLength(pos: Pos)
  extends Error("new array length must be an integer", pos)

class ThisInStaticFuncError(pos: Pos)
  extends Error("can not use this in static function", pos)

class UndeclVarError(v: String, pos: Pos)
  extends Error(s"undeclared variable '$v'", pos)

class NotClassError(typ: Type, pos: Pos)
  extends Error(s"$typ is not a class type", pos)

class FieldNotFoundError(field: String, clazz: ClassType, pos: Pos)
  extends Error(s"field '$field' not found in '$clazz'", pos)

class FieldNotAccessError(field: String, clazz: ClassType, pos: Pos)
  extends Error(s"field '$field' of '$clazz' not accessible here", pos)

class RefNonStaticError(field: String, method: String, pos: Pos)
  extends Error(s"can not reference a non-static field '$field' from static method '$method'", pos)

class NotClassFieldError(field: String, typ: Type, pos: Pos)
  extends Error(s"cannot access field '$field' from '$typ'", pos)

class NotClassMethodError(field: String, clazz: ClassType, pos: Pos)
  extends Error(s"'$field' is not a method in class '$clazz'", pos)

class BadArgCountError(method: String, expected: Int, actual: Int, pos: Pos)
  extends Error(s"function '$method' expects $expected argument(s) but $actual given", pos)

class BadArgTypeError(k: Int, expected: Type, actual: Type, pos: Pos)
  extends Error(s"incompatible argument $k: $actual given, $expected expected", pos)

class BadLengthArgError(count: Int, pos: Pos)
  extends Error(s"function 'length' expects 0 argument(s) but $count given", pos)
