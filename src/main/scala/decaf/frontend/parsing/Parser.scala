package decaf.frontend.parsing

import java.io.Reader

import decaf.frontend.core.Trees._
import decaf.frontend.util.TupleSeq

class TokenParsers extends Lexer with TupleSeq {
  def intLit: Parser[IntLit] = positioned((decimal | hex) ^^ IntLit)

  def boolLit: Parser[BoolLit] = positioned(boolean ^^ BoolLit)

  def stringLit: Parser[StringLit] = positioned(quotedString ^^ StringLit)

  def nullLit: Parser[NullLit] = positioned("null" ^^^ NullLit())

  def unit: Parser[Unit] = "(" ~ ")" ^^^ ()

  def id: Parser[Id] = positioned(identifier ^^ Id)
}

class TypeParsers extends TokenParsers {
  def typ1: Parser[Type] = positioned("int" ^^^ TInt() | "bool" ^^^ TBool() | "string" ^^^ TString()
    | "void" ^^^ TVoid() | "class" ~> id ^^ TClass)

  def typ: Parser[Type] = typ1 ~ ("[" ~ "]").* ^^ {
    case elemType ~ dims => dims.foldLeft(elemType) { (t, _) => TArray(t) }
  }
}

class ExprParsers extends TypeParsers {
  def lit: Parser[Lit] = intLit | boolLit | stringLit | nullLit

  def expr1: Parser[Expr] = lit | positioned("this" ^^^ This() | "(" ~> expr <~ ")"
    | "ReadInteger" ~ unit ^^^ ReadInt() | "ReadLine" ~ unit ^^^ ReadLine()
    | "new" ~> id <~ unit ^^ NewClass | "new" ~> typ ~ ("[" ~> expr <~ "]") ^^ NewArray
    | "instanceof" ~ "(" ~> expr ~ ("," ~> id <~ ")") ^^ ClassTest
    | ("(" ~ "class" ~> id <~ ")") ~ expr1 ^^ { case t ~ e => ClassCast(e, t) }
    | id ~ ("(" ~> exprList <~ ")") ^^ { case f ~ xs => Call(f, xs) } | id
  )

  def access: Parser[Expr => Expr] = "[" ~> expr <~ "]" ^^ { index => (e: Expr) => IndexSel(e, index) } |
    "." ~> id ~ ("(" ~> exprList <~ ")") ^^ { case f ~ xs => (e: Expr) => Call(e, f, xs) } |
    "." ~> id ^^ { case x => (e: Expr) => FieldSel(e, x) }

  def expr2: Parser[Expr] = positioned(expr1 ~ access.* ^^ {
    case recv ~ ps => ps.foldLeft(recv) {
      case (e, f) => f(e)
    }
  })

  def opUn: Parser[UnaryOp] = positioned("-" ^^^ NEG() | "!" ^^^ NOT())

  def expr3: Parser[Expr] = opUn.* ~ expr2 ^^ {
    case ops ~ e => ops.foldRight(e) {
      UnaryExpr
    }
  }

  private def mkBinaryExprParser[Op, Term](maker: (Op, Term, Term) => Term,
                                           op: Parser[Op], term: Parser[Term]): Parser[Term] =
    term ~ (op ~ term).* ^^ {
      case t ~ ps => ps.foldLeft(t) { case (l, op ~ r) => maker(op, l, r) }
    }

  def opMul: Parser[BinaryOp] = positioned("*" ^^^ MUL() | "/" ^^^ DIV() | "%" ^^^ MOD())

  def expr4: Parser[Expr] = mkBinaryExprParser(BinaryExpr, opMul, expr3)

  def opAdd: Parser[BinaryOp] = positioned("+" ^^^ ADD() | "-" ^^^ SUB())

  def expr5: Parser[Expr] = mkBinaryExprParser(BinaryExpr, opAdd, expr4)

  def opCmp: Parser[BinaryOp] = positioned("<=" ^^^ LE() | ">=" ^^^ GE() | "<" ^^^ LT() | ">" ^^^ GT())

  def expr6: Parser[Expr] = mkBinaryExprParser(BinaryExpr, opCmp, expr5)

  def opEql: Parser[BinaryOp] = positioned("==" ^^^ EQ() | "!=" ^^^ NE())

  def expr7: Parser[Expr] = mkBinaryExprParser(BinaryExpr, opEql, expr6)

  def opAnd: Parser[BinaryOp] = positioned("&&" ^^^ AND())

  def expr8: Parser[Expr] = mkBinaryExprParser(BinaryExpr, opAnd, expr7)

  def opOr: Parser[BinaryOp] = positioned("||" ^^^ OR())

  def expr9: Parser[Expr] = mkBinaryExprParser(BinaryExpr, opOr, expr8)

  def expr: Parser[Expr] = expr9

  def exprList: Parser[List[Expr]] = repsep(expr, ",")
}

class StmtParsers extends ExprParsers {
  def localVarDef: Parser[LocalVarDef] = positioned(typ ~ id <~ ";" ^^ LocalVarDef)

  def assign: Parser[Assign] = positioned(expr ~ ("=" ~> expr) <~ ";" ^^ Assign)

  def exprEval: Parser[ExprEval] = positioned(expr <~ ";" ^^ ExprEval)

  def skip: Parser[Skip] = positioned(";" ^^^ Skip())

  def simpleStmt: Parser[SimpleStmt] = assign | exprEval | skip

  def ifStmt: Parser[If] = positioned("if" ~> ("(" ~> expr <~ ")") ~ stmt ~ ("else" ~> stmt).? ^^ If)

  def whileStmt: Parser[While] = positioned("while" ~> expr ~ stmt ^^ While)

  def forStmt: Parser[For] = positioned("for" ~> (("(" ~> simpleStmt <~ ";") ~ (expr <~ ";") ~ (simpleStmt <~ ")")
    ~ stmt) ^^ For)

  def breakStmt: Parser[Break] = positioned("break" ~ ";" ^^^ Break())

  def returnStmt: Parser[Return] = positioned("return" ~> expr.? <~ ";" ^^ Return)

  def printStmt: Parser[Print] = positioned("Print" ~ "(" ~> exprList <~ ")" ~ ";" ^^ Print)

  def block: Parser[Block] = positioned("{" ~> stmt.* <~ "}" ^^ Block)

  def stmt: Parser[Stmt] = localVarDef | simpleStmt | ifStmt | whileStmt | forStmt | breakStmt | returnStmt |
    printStmt | block
}

class TopLevelParsers extends StmtParsers {
  private def typed: Parser[VarDef] = positioned(typ ~ id ^^ VarDef)

  def varDef: Parser[VarDef] = typed <~ ';'

  def sig: Parser[Sig] = positioned(typ ~ id ~ ("(" ~> repsep(typed, ",") <~ ")") ^^ Sig)

  def methodDef: Parser[MethodDef] = positioned(("static".? ^^ {
    _.isDefined
  }) ~ sig ~ block ^^ MethodDef)

  def field: Parser[Field] = varDef | methodDef

  def classDef: Parser[ClassDef] = positioned("class" ~> id ~ ("extends" ~> id).? ~ ("{" ~> field.* <~ "}") ^^ ClassDef)

  def topLevel: Parser[TopLevel] = positioned(classDef.* ^^ TopLevel)
}

class Parser extends TopLevelParsers {
  def parse(file: Reader): ParseResult[TopLevel] = parseAll(topLevel, file)
}