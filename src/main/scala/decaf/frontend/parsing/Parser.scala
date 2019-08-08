package decaf.frontend.parsing

import java.io.Reader

import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode._

import scala.util.parsing.input.Positional

class TokenParsers extends Lexer {
  def intLit: Parser[IntLit] = positioned((decimal | hex) ^^ {
    IntLit(_)
  })

  def boolLit: Parser[BoolLit] = positioned(boolean ^^ {
    BoolLit(_)
  })

  def stringLit: Parser[StringLit] = positioned(quotedString ^^ {
    StringLit(_)
  })

  def nullLit: Parser[NullLit] = positioned("null" ^^^ NullLit())

  def unit: Parser[Unit] = "(" ~ ")" ^^^ ()

  def id: Parser[Id] = positioned(identifier ^^ Id)
}

class TypeParsers extends TokenParsers {
  def typ1: Parser[TypeLit] = positioned("int" ^^^ TInt() | "bool" ^^^ TBool()
    | "string" ^^^ TString() | "void" ^^^ TVoid() | "class" ~> id ^^ {
    TClass(_)
  })

  def typ: Parser[TypeLit] = typ1 ~ ("[" ~ "]").* ^^ {
    case elemType ~ dims => dims.foldLeft(elemType) { (t, _) => TArray(t) }
  }
}

class ExprParsers extends TypeParsers {
  def lit: Parser[Lit] = intLit | boolLit | stringLit | nullLit

  def expr1: Parser[Expr] = lit |
    positioned("this" ^^^ This() |
      "(" ~> expr <~ ")" |
      "ReadInteger" ~ unit ^^^ ReadInt() |
      "ReadLine" ~ unit ^^^ ReadLine() |
      "new" ~> id <~ unit ^^ {
        NewClass(_)
      } |
      "new" ~> typ ~ ("[" ~> expr <~ "]") ^^ {
        case t ~ e => NewArray(t, e)
      } |
      "instanceof" ~ "(" ~> expr ~ ("," ~> id <~ ")") ^^ {
        case e ~ t => ClassTest(e, t)
      } |
      ("(" ~ "class" ~> id <~ ")") ~ expr1 ^^ { case t ~ e => ClassCast(e, t) } |
      id ~ ("(" ~> exprList <~ ")") ^^ { case f ~ xs => Call(None, f, xs) } |
      id ^^ {
        Var(_)
      })

  def access: Parser[Expr => Expr] = "[" ~> expr <~ "]" ^^ { index => (e: Expr) => IndexSel(e, index) } |
    "." ~> id ~ ("(" ~> exprList <~ ")") ^^ { case f ~ xs => (e: Expr) => Call(Some(e), f, xs) } |
    "." ~> id ^^ { x => (e: Expr) => FieldSel(e, x) }

  def expr2: Parser[Expr] = positioned(expr1 ~ access.* ^^ {
    case recv ~ ps => ps.foldLeft(recv) {
      case (e, f) => f(e)
    }
  })

  def opUn: Parser[UnaryOp] = "-" ^^^ NEG | "!" ^^^ NOT

  def expr3: Parser[Expr] = opUn.* ~ expr2 ^^ {
    case ops ~ e => ops.foldRight(e) {
      UnaryExpr(_, _)
    }
  }

  case class PosOp(self: BinaryOp) extends Positional

  private def mkBinaryExprParser(op: Parser[PosOp], term: Parser[Expr]): Parser[Expr] =
    term ~ (op ~ term).* ^^ {
      case t ~ ps => ps.foldLeft(t) { case (l, op ~ r) => BinaryExpr(op.self, l, r).setPos(op.pos) }
    }

  def opMul: Parser[PosOp] = positioned(
    ("*" ^^^ MUL | "/" ^^^ DIV | "%" ^^^ MOD) map PosOp)

  def expr4: Parser[Expr] = mkBinaryExprParser(opMul, expr3)

  def opAdd: Parser[PosOp] = positioned(
    ("+" ^^^ ADD | "-" ^^^ SUB) map PosOp)

  def expr5: Parser[Expr] = mkBinaryExprParser(opAdd, expr4)

  def opCmp: Parser[PosOp] = positioned(
    ("<=" ^^^ LE | ">=" ^^^ GE | "<" ^^^ LT | ">" ^^^ GT) map PosOp)

  def expr6: Parser[Expr] = mkBinaryExprParser(opCmp, expr5)

  def opEql: Parser[PosOp] = positioned(
    ("==" ^^^ EQ | "!=" ^^^ NE) map PosOp)

  def expr7: Parser[Expr] = mkBinaryExprParser(opEql, expr6)

  def opAnd: Parser[PosOp] = positioned("&&" ^^^ AND map PosOp)

  def expr8: Parser[Expr] = mkBinaryExprParser(opAnd, expr7)

  def opOr: Parser[PosOp] = positioned("||" ^^^ OR map PosOp)

  def expr9: Parser[Expr] = mkBinaryExprParser(opOr, expr8)

  def expr: Parser[Expr] = expr9

  def exprList: Parser[List[Expr]] = repsep(expr, ",")
}

class StmtParsers extends ExprParsers {
  def localVarDef: Parser[LocalVarDef] = positioned(typ ~ id <~ ";" ^^ {
    case t ~ i => LocalVarDef(t, i)
  })

  def block: Parser[Block] = positioned("{" ~> stmt.* <~ "}" ^^ {
    Block(_)
  })

  def assign: Parser[Assign] = positioned(expr ~ ("=" ~> expr) <~ ";" ^^ {
    case l ~ e => Assign(l, e)
  })

  def exprEval: Parser[ExprEval] = positioned(expr <~ ";" ^^ {
    ExprEval(_)
  })

  def skip: Parser[Skip] = positioned(";" ^^^ {
    Skip()
  })

  def simpleStmt: Parser[SimpleStmt] = assign | exprEval | skip

  def ifStmt: Parser[If] = positioned("if" ~> ("(" ~> expr <~ ")") ~ stmt ~ ("else" ~> stmt).? ^^ {
    case b ~ t ~ f => If(b, t, f)
  })

  def whileStmt: Parser[While] = positioned("while" ~> expr ~ stmt ^^ {
    case b ~ s => While(b, s)
  })

  def forStmt: Parser[For] = positioned("for" ~> (("(" ~> simpleStmt <~ ";") ~ (expr <~ ";") ~ (simpleStmt <~ ")")
    ~ stmt) ^^ {
    case i ~ b ~ u ~ s => For(i, b, u, s)
  })

  def breakStmt: Parser[Break] = positioned("break" ~ ";" ^^^ {
    Break()
  })

  def returnStmt: Parser[Return] = positioned("return" ~> expr.? <~ ";" ^^ {
    Return(_)
  })

  def printStmt: Parser[Print] = positioned("Print" ~ "(" ~> exprList <~ ")" ~ ";" ^^ {
    Print(_)
  })

  def controlStmt: Parser[Stmt] = simpleStmt | ifStmt | whileStmt | forStmt | breakStmt | returnStmt | printStmt

  def stmt: Parser[Stmt] = localVarDef | block | controlStmt
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
    case i ~ e ~ fs => ClassDef(i, e, fs)
  })

  def topLevel: Parser[Tree] = positioned(classDef.* ^^ {
    TopLevel(_)
  })
}

class Parser extends TopLevelParsers {
  def parse(file: Reader): ParseResult[Tree] = parseAll(topLevel, file)
}