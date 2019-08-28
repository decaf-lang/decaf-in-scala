package decaf.printing

import decaf.parsing.Util.quote
import decaf.tree.SyntaxTree._
import decaf.tree.TreeNode
import decaf.tree.TreeNode.{Id, Node, Op}

/**
  * PA1 output.
  */
object PA1Tree {

  def pretty(node: Node)(implicit printer: IndentPrinter): Unit = node match {
    case TopLevel(classes) =>
      printer.writeln("program")
      printer.withIndent { classes.foreach(pretty) }
    case ClassDef(id, parent, fields) =>
      printer.writeln(s"class $id ${ parent.getOrElse("<empty>") }")
      printer.withIndent { fields.foreach(pretty) }
    case VarDef(typeLit, id) => printer.writeln(s"vardef ${ id } ${ prettyType(typeLit) }")
    case MethodDef(isStatic, returnType, id, params, body) =>
      printer.writeln(s"${ if (isStatic) "static " else "" }func $id ${ prettyType(returnType) }")
      printer.withIndent {
        printer.writeln("formals")
        printer.withIndent { params.foreach(pretty) }
        pretty(body)
      }

    case t: TypeLit => printer.writeln(prettyType(t))

    case LocalVarDef(typeLit, id) => printer.writeln(s"vardef ${ id } ${ prettyType(typeLit) }")
    case Block(stmts) =>
      printer.writeln("stmtblock")
      printer.withIndent { stmts.foreach(pretty) }
    case Assign(lhs, rhs) =>
      printer.writeln("assign")
      printer.withIndent {
        pretty(lhs)
        pretty(rhs)
      }
    case ExprEval(expr) => pretty(expr)
    case Skip() => // print nothing
    case If(cond, trueBranch, falseBranch) =>
      printer.writeln("if")
      printer.withIndent {
        pretty(cond)
        pretty(trueBranch)
      }
      if (!falseBranch.isEmpty) {
        printer.writeln("else")
        printer.withIndent { pretty(falseBranch) }
      }
    case While(cond, body) =>
      printer.writeln("while")
      printer.withIndent {
        pretty(cond)
        if (!body.isEmpty) pretty(body)
      }
    case For(init, cond, update, body) =>
      printer.writeln("for")
      printer.withIndent {
        pretty(init)
        pretty(cond)
        pretty(update)
        pretty(body)
      }
    case Break() => printer.writeln("break")
    case Return(expr) =>
      printer.writeln("return")
      expr match {
        case Some(e) => printer.withIndent { pretty(e) }
        case None => // print nothing
      }
    case Print(exprs) =>
      printer.writeln("print")
      printer.withIndent { exprs.foreach(pretty) }

    case lit: Lit => printer.writeln(prettyLit(lit))
    case IndexSel(array, index) =>
      printer.writeln("arrref")
      printer.withIndent {
        pretty(array)
        pretty(index)
      }
    case VarSel(receiver, v) =>
      printer.writeln(s"varref $v")
      printer.withIndent {
        receiver match {
          case Some(r) => pretty(r)
          case None => // print nothing
        }
      }
    case Call(receiver, method, args) =>
      printer.writeln(s"call $method")
      printer.withIndent {
        receiver match {
          case Some(r) => pretty(r)
          case None => printer.writeln("<empty>")
        }
        args.foreach(pretty)
      }

    case This() => printer.writeln("this")
    case UnaryExpr(op, operand) =>
      printer.writeln(prettyOp(op))
      printer.withIndent { pretty(operand) }
    case BinaryExpr(op, lhs, rhs) =>
      printer.writeln(prettyOp(op))
      printer.withIndent {
        pretty(lhs)
        pretty(rhs)
      }
    case ReadInt() => printer.writeln("readint")
    case ReadLine() => printer.writeln("readline")
    case NewClass(id) => printer.writeln(s"newobj $id")
    case NewArray(elemType, length) =>
      printer.writeln(s"newarray ${ prettyType(elemType) }")
      printer.withIndent { pretty(length) }
    case ClassTest(obj, id) =>
      printer.writeln("instanceof")
      printer.withIndent {
        pretty(obj)
        pretty(id)
      }
    case ClassCast(obj, id) =>
      printer.writeln("classcast")
      printer.withIndent {
        pretty(id)
        pretty(obj)
      }

    case Id(name) => printer.writeln(name)
  }

  def prettyType(typeLit: TypeLit): String = typeLit match {
    case TInt() => "inttype"
    case TBool() => "booltype"
    case TString() => "stringtype"
    case TVoid() => "voidtype"
    case TClass(id) => s"classtype $id"
    case TArray(elemType) => s"arrtype ${ prettyType(elemType) }"
  }

  def prettyLit(lit: Lit): String = lit match {
    case BoolLit(value) => s"boolconst $value"
    case IntLit(value) => s"intconst $value"
    case NullLit() => "null"
    case StringLit(value) => s"stringconst ${ quote(value) }"
  }

  def prettyOp(op: Op): String = op match {
    case TreeNode.NEG => "neg"
    case TreeNode.NOT => "not"
    case TreeNode.ADD => "add"
    case TreeNode.SUB => "sub"
    case TreeNode.MUL => "mul"
    case TreeNode.DIV => "div"
    case TreeNode.MOD => "mod"
    case TreeNode.AND => "and"
    case TreeNode.OR => "or"
    case TreeNode.EQ => "equ"
    case TreeNode.NE => "neq"
    case TreeNode.LT => "les"
    case TreeNode.LE => "leq"
    case TreeNode.GT => "gtr"
    case TreeNode.GE => "geq"
  }
}
