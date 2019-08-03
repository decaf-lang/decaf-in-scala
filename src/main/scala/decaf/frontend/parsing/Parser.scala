package decaf.frontend.parsing

import java.io.Reader

import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode

import scala.util.parsing.input.Positional

class TokenParsers extends Lexer {
  def intLit: Parser[IntLit] = positioned((decimal | hex) ^^ {
    TreeNode.IntLit(_)
  })

  def boolLit: Parser[BoolLit] = positioned(boolean ^^ {
    TreeNode.BoolLit(_)
  })

  def stringLit: Parser[StringLit] = positioned(quotedString ^^ {
    TreeNode.StringLit(_)
  })

  def nullLit: Parser[NullLit] = positioned("null" ^^^ TreeNode.NullLit())

  def unit: Parser[Unit] = "(" ~ ")" ^^^ ()

  def id: Parser[Id] = positioned(identifier ^^ TreeNode.Id)
}

class TypeParsers extends TokenParsers {
  def typ1: Parser[TypeLit] = positioned("int" ^^^ TreeNode.TInt() | "bool" ^^^ TreeNode.TBool()
    | "string" ^^^ TreeNode.TString() | "void" ^^^ TreeNode.TVoid() | "class" ~> id ^^ {
    TreeNode.TClass(_)
  })

  def typ: Parser[TypeLit] = typ1 ~ ("[" ~ "]").* ^^ {
    case elemType ~ dims => dims.foldLeft(elemType) { (t, _) => TreeNode.TArray(t) }
  }
}

class ExprParsers extends TypeParsers {
  def lit: Parser[Lit] = intLit | boolLit | stringLit | nullLit

  def expr1: Parser[Expr] = lit |
    positioned("this" ^^^ TreeNode.This() |
      "(" ~> expr <~ ")" |
      "ReadInteger" ~ unit ^^^ TreeNode.ReadInt() |
      "ReadLine" ~ unit ^^^ TreeNode.ReadLine() |
      "new" ~> id <~ unit ^^ {
        TreeNode.NewClass(_)
      } |
      "new" ~> typ ~ ("[" ~> expr <~ "]") ^^ {
        case t ~ e => TreeNode.NewArray(t, e)
      } |
      "instanceof" ~ "(" ~> expr ~ ("," ~> id <~ ")") ^^ {
        case e ~ t => TreeNode.ClassTest(e, t)
      } |
      ("(" ~ "class" ~> id <~ ")") ~ expr1 ^^ { case t ~ e => TreeNode.ClassCast(e, t) } |
      id ~ ("(" ~> exprList <~ ")") ^^ { case f ~ xs => TreeNode.Call(None, f, xs) } |
      id ^^ {
        TreeNode.Var(_)
      })

  def access: Parser[Expr => Expr] = "[" ~> expr <~ "]" ^^ { index => (e: Expr) => TreeNode.IndexSel(e, index) } |
    "." ~> id ~ ("(" ~> exprList <~ ")") ^^ { case f ~ xs => (e: Expr) => TreeNode.Call(Some(e), f, xs) } |
    "." ~> id ^^ { x => (e: Expr) => TreeNode.FieldSel(e, x) }

  def expr2: Parser[Expr] = positioned(expr1 ~ access.* ^^ {
    case recv ~ ps => ps.foldLeft(recv) {
      case (e, f) => f(e)
    }
  })

  def opUn: Parser[TreeNode.UnaryOp] = "-" ^^^ TreeNode.NEG | "!" ^^^ TreeNode.NOT

  def expr3: Parser[Expr] = opUn.* ~ expr2 ^^ {
    case ops ~ e => ops.foldRight(e) {
      TreeNode.UnaryExpr(_, _)
    }
  }

  case class BinaryOp(self: TreeNode.BinaryOp) extends Positional

  private def mkBinaryExprParser(op: Parser[BinaryOp], term: Parser[Expr]): Parser[Expr] =
    term ~ (op ~ term).* ^^ {
      case t ~ ps => ps.foldLeft(t) { case (l, op ~ r) => TreeNode.BinaryExpr(op.self, l, r).setPos(op.pos) }
    }

  def opMul: Parser[BinaryOp] = positioned(
    ("*" ^^^ TreeNode.MUL | "/" ^^^ TreeNode.DIV | "%" ^^^ TreeNode.MOD) map BinaryOp)

  def expr4: Parser[Expr] = mkBinaryExprParser(opMul, expr3)

  def opAdd: Parser[BinaryOp] = positioned(
    ("+" ^^^ TreeNode.ADD | "-" ^^^ TreeNode.SUB) map BinaryOp)

  def expr5: Parser[Expr] = mkBinaryExprParser(opAdd, expr4)

  def opCmp: Parser[BinaryOp] = positioned(
    ("<=" ^^^ TreeNode.LE | ">=" ^^^ TreeNode.GE | "<" ^^^ TreeNode.LT | ">" ^^^ TreeNode.GT) map BinaryOp)

  def expr6: Parser[Expr] = mkBinaryExprParser(opCmp, expr5)

  def opEql: Parser[BinaryOp] = positioned(
    ("==" ^^^ TreeNode.EQ | "!=" ^^^ TreeNode.NE) map BinaryOp)

  def expr7: Parser[Expr] = mkBinaryExprParser(opEql, expr6)

  def opAnd: Parser[BinaryOp] = positioned("&&" ^^^ TreeNode.AND map BinaryOp)

  def expr8: Parser[Expr] = mkBinaryExprParser(opAnd, expr7)

  def opOr: Parser[BinaryOp] = positioned("||" ^^^ TreeNode.OR map BinaryOp)

  def expr9: Parser[Expr] = mkBinaryExprParser(opOr, expr8)

  def expr: Parser[Expr] = expr9

  def exprList: Parser[List[Expr]] = repsep(expr, ",")
}

class StmtParsers extends ExprParsers {
  def localVarDef: Parser[LocalVarDef] = positioned(typ ~ id <~ ";" ^^ {
    case t ~ i => LocalVarDef(t, i)
  })

  def block: Parser[Block] = positioned("{" ~> stmt.* <~ "}" ^^ Block)

  def assign: Parser[Assign] = positioned(expr ~ ("=" ~> expr) <~ ";" ^^ {
    case l ~ e => TreeNode.Assign(l, e)
  })

  def exprEval: Parser[ExprEval] = positioned(expr <~ ";" ^^ {
    TreeNode.ExprEval(_)
  })

  def skip: Parser[Skip] = positioned(";" ^^^ TreeNode.Skip())

  def simpleStmt: Parser[SimpleStmt] = assign | exprEval | skip

  def ifStmt: Parser[If] = positioned("if" ~> ("(" ~> expr <~ ")") ~ stmt ~ ("else" ~> stmt).? ^^ {
    case b ~ t ~ f => TreeNode.If(b, t, f)
  })

  def whileStmt: Parser[While] = positioned("while" ~> expr ~ stmt ^^ {
    case b ~ s => TreeNode.While(b, s)
  })

  def forStmt: Parser[For] = positioned("for" ~> (("(" ~> simpleStmt <~ ";") ~ (expr <~ ";") ~ (simpleStmt <~ ")")
    ~ stmt) ^^ {
    case i ~ b ~ u ~ s => TreeNode.For(i, b, u, s)
  })

  def breakStmt: Parser[Break] = positioned("break" ~ ";" ^^^ TreeNode.Break())

  def returnStmt: Parser[Return] = positioned("return" ~> expr.? <~ ";" ^^ {
    TreeNode.Return(_)
  })

  def printStmt: Parser[Print] = positioned("Print" ~ "(" ~> exprList <~ ")" ~ ";" ^^ {
    TreeNode.Print(_)
  })

  def controlStmt: Parser[ControlStmt] = simpleStmt | ifStmt | whileStmt | forStmt | breakStmt | returnStmt | printStmt

  def stmt: Parser[Stmt] = localVarDef | block | controlStmt ^^ ControlStmtWarp
}

class TopLevelParsers extends StmtParsers {
  private def typed: Parser[VarDef] = positioned(typ ~ id ^^ {
    case t ~ i => VarDef(t, i)
  })

  def varDef: Parser[VarDef] = typed <~ ';'

  def methodDef: Parser[MethodDef] = positioned(("static".? ^^ {
    _.isDefined
  }) ~ typ ~ id ~ ("(" ~> repsep(typed, ",") <~ ")") ~ block ^^ {
    case b ~ t ~ f ~ ts ~ s => MethodDef(b, t, f, ts, s)
  })

  def field: Parser[Field] = varDef | methodDef

  def classDef: Parser[ClassDef] = positioned("class" ~> id ~ ("extends" ~> id).? ~ ("{" ~> field.* <~ "}") ^^ {
    case i ~ e ~ fs => TreeNode.ClassDef(i, e, fs)
  })

  def topLevel: Parser[Tree] = positioned(classDef.* ^^ {
    TreeNode.TopLevel(_)
  })
}

class Parser extends TopLevelParsers {
  def parse(file: Reader): ParseResult[Tree] = parseAll(topLevel, file)
}