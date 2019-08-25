package decaf.frontend.parsing

import java.io.{InputStream, InputStreamReader}

import decaf.driver.{Config, Phase}
import decaf.error.SyntaxError
import decaf.frontend.parsing.Tokens._
import decaf.frontend.printing.{IndentPrinter, PA1Tree, PrettyTree}
import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode
import decaf.frontend.tree.TreeNode.{Id, Op}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

class BasicParsers extends Parsers {
  override type Elem = Token

  class DecafReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def rest: Reader[Token] = new DecafReader(tokens.tail)

    override def pos: Position = if (!atEnd) first.pos else NoPosition

    override def atEnd: Boolean = tokens.isEmpty
  }

  // For every keyword, create a parser which accepts that keyword, using implicit conversion.
  implicit def __keyword__(keyword: Keyword): Parser[Keyword] = positioned {
    accept(s"keyword '${ keyword.name }'", { case k: Keyword if k == keyword => keyword })
  }

  def intLit: Parser[IntLit] = positioned {
    accept("integer literal", { case INT_LIT(value) => IntLit(value) })
  }

  def boolLit: Parser[BoolLit] = positioned { TRUE ^^^ BoolLit(true) | FALSE ^^^ BoolLit(false) }

  def stringLit: Parser[StringLit] = positioned {
    accept("string literal", { case STRING_LIT(value) => StringLit(value) })
  }

  def nullLit: Parser[NullLit] = positioned { NULL ^^^ NullLit() }

  def id: Parser[Id] = positioned(accept("identifier", { case IDENT(name) => Id(name) }))

  def unit: Parser[Unit] = LPAREN ~! RPAREN ^^^ ()
}

class TypeParsers extends BasicParsers {
  def typ1: Parser[TypeLit] = positioned {
    INT ^^^ TInt() | BOOL ^^^ TBool() | STRING ^^^ TString() | VOID ^^^ TVoid() | CLASS ~>! id ^^ { TClass(_) }
  }

  def typ: Parser[TypeLit] = typ1 ~ (LBRACK ~ RBRACK).* ^^ {
    case elemType ~ dims => dims.foldLeft(elemType) { (t, _) => TArray(t) }
  }
}

class ExprParsers extends TypeParsers {
  def lit: Parser[Lit] = intLit | boolLit | stringLit | nullLit

  def expr1: Parser[Expr] = lit | positioned {
    THIS ^^^ This() |
      LPAREN ~>! (expr <~ RPAREN | (CLASS ~> id <~ RPAREN) ~ expr1 ^^ { case t ~ e => ClassCast(e, t) }) |
      READ_INTEGER ~! unit ^^^ ReadInt() |
      READ_LINE ~! unit ^^^ ReadLine() |
      NEW ~>! (id <~ unit ^^ { NewClass(_) } | typ ~ (LBRACK ~> expr <~ RBRACK) ^^ { case t ~ e => NewArray(t, e) }) |
      INSTANCE_OF ~! LPAREN ~> expr ~ (COMMA ~> id <~ RPAREN) ^^ { case e ~ t => ClassTest(e, t) } |
      id ~! (LPAREN ~>! exprList <~ RPAREN).? ^^ {
        case f ~ Some(xs) => Call(None, f, xs)
        case x ~ None => VarSel(None, x)
      }
  }

  def access: Parser[Expr => Expr] =
    LBRACK ~>! expr <~ RBRACK ^^ { index => (e: Expr) => IndexSel(e, index) } |
      DOT ~>! id ~ (LPAREN ~>! exprList <~ RPAREN).? ^^ {
        case f ~ Some(xs) => (e: Expr) => Call(Some(e), f, xs)
        case x ~ None => (e: Expr) => VarSel(Some(e), x)
      }

  def expr2: Parser[Expr] = positioned(expr1 ~! access.* ^^ {
    case recv ~ ps => ps.foldLeft(recv) { case (e, f) => f(e) }
  })

  // TODO distinguish unary and binary
  implicit def __op__(operator: Operator): Parser[Op] = __keyword__(operator) ^^^ operator.op

  def opUn: Parser[Op] = NEG | NOT

  def expr3: Parser[Expr] = opUn.+ ~! expr2 ^^ {
    case ops ~ e => ops.foldRight(e) { UnaryExpr(_, _) }
  } | expr2

  case class PosOp(self: Op) extends Positional

  private def mkBinaryExprParser(op: Parser[PosOp], term: Parser[Expr]): Parser[Expr] =
    term ~! (op ~! term).* ^^ {
      case t ~ ps => ps.foldLeft(t) { case (l, op ~ r) => BinaryExpr(op.self, l, r).setPos(op.pos) }
    }

  def opMul: Parser[PosOp] = positioned { (MUL | DIV | MOD) map PosOp }

  def expr4: Parser[Expr] = mkBinaryExprParser(opMul, expr3)

  def opAdd: Parser[PosOp] = positioned { (ADD | NEG ^^^ TreeNode.SUB) map PosOp }

  def expr5: Parser[Expr] = mkBinaryExprParser(opAdd, expr4)

  def opCmp: Parser[PosOp] = positioned { (LE | GE | LT | GT) map PosOp }

  def expr6: Parser[Expr] = mkBinaryExprParser(opCmp, expr5)

  def opEql: Parser[PosOp] = positioned { (EQ | NE) map PosOp }

  def expr7: Parser[Expr] = mkBinaryExprParser(opEql, expr6)

  def opAnd: Parser[PosOp] = positioned { AND map PosOp }

  def expr8: Parser[Expr] = mkBinaryExprParser(opAnd, expr7)

  def opOr: Parser[PosOp] = positioned { OR map PosOp }

  def expr9: Parser[Expr] = mkBinaryExprParser(opOr, expr8)

  def expr: Parser[Expr] = expr9

  def exprList: Parser[List[Expr]] = repsep(expr, COMMA)
}

class StmtParsers extends ExprParsers {

  case class Typed(typeLit: TypeLit, id: Id) extends Positional

  def typed: Parser[Typed] = positioned(typ ~ id ^^ { case t ~ i => Typed(t, i) })

  def localVarDef: Parser[LocalVarDef] = positioned(typed <~! SEMI ^^ { r => LocalVarDef(r.typeLit, r.id) })

  def block: Parser[Block] = positioned(LBRACE ~>! stmt.* <~ RBRACE ^^ { Block(_) })

  def assignOrExprEval: Parser[SimpleStmt] = positioned {
    expr ~! (ASSIGN ~>! expr).? ^^ {
      case l ~ Some(e) => Assign(l.asInstanceOf[LValue], e)
      case e ~ None => ExprEval(e)
    }
  }

  def simpleStmt: Parser[SimpleStmt] = positioned {
    assignOrExprEval.? ^^ {
      case None => Skip()
      case Some(s) => s
    }
  }

  def ifStmt: Parser[If] = positioned(IF ~>! (LPAREN ~> expr <~ RPAREN) ~ stmt ~ (ELSE ~>! stmt).? ^^ {
    case b ~ t ~ f => If(b, t, f.getOrElse(Block()))
  })

  def whileStmt: Parser[While] = positioned(WHILE ~>! expr ~ stmt ^^ {
    case b ~ s => While(b, s)
  })

  def forStmt: Parser[For] = positioned {
    FOR ~>! ((LPAREN ~> simpleStmt <~ SEMI) ~ (expr <~ SEMI) ~ (simpleStmt <~ RPAREN) ~ stmt) ^^ {
      case i ~ b ~ u ~ s => For(i, b, u, s)
    }
  }

  def breakStmt: Parser[Break] = positioned(BREAK ~! SEMI ^^^ { Break() })

  def returnStmt: Parser[Return] = positioned(RETURN ~>! expr.? <~ SEMI ^^ { Return(_) })

  def printStmt: Parser[Print] = positioned(PRINT ~! LPAREN ~> exprList <~ RPAREN ~ SEMI ^^ { Print(_) })

  def controlStmt: Parser[Stmt] = ifStmt | whileStmt | forStmt | breakStmt | returnStmt | printStmt |
    assignOrExprEval <~! SEMI | positioned(SEMI ^^^ Skip())

  def stmt: Parser[Stmt] = block | localVarDef | controlStmt
}

class TopLevelParsers extends StmtParsers {
  def varDef: Parser[VarDef] = positioned(typed <~ SEMI ^^ { r => VarDef(r.typeLit, r.id) })

  def methodDef: Parser[MethodDef] = positioned(
    (STATIC.? ^^ { _.isDefined }) ~ typ ~ id ~! (LPAREN ~>! repsep(typed, COMMA) <~ RPAREN) ~ block ^^ {
      case b ~ t ~ f ~ ts ~ s => MethodDef(b, t, f, ts.map { r => LocalVarDef(r.typeLit, r.id) }, s)
    })

  def field: Parser[Field] = varDef | methodDef

  def classDef: Parser[ClassDef] = positioned(CLASS ~>! id ~ (EXTENDS ~>! id).? ~ (LBRACE ~> field.* <~ RBRACE) ^^ {
    case i ~ e ~ fs => ClassDef(i, e, fs)
  })

  def topLevel: Parser[Tree] = positioned { phrase(classDef.* ^^ { TopLevel(_) }) }
}

class Parser extends Phase[InputStream, Tree]("parser") {
  val lexer = new Lexer
  val parser = new TopLevelParsers

  def parse[T](p: parser.Parser[T], in: String): parser.ParseResult[T] = {
    lexer.parse(lexer.tokens, in) match {
      case lexer.Success(tokens, _) =>
        val reader = new parser.DecafReader(tokens)
        p(reader)
      case f: lexer.NoSuccess =>
        println(s"Lexer:${ f.next.pos }:${ f.msg }:\n${ f.next.pos.longString }")
        parser.Error("Lexer error", null)
    }
  }

  override def transform(in: InputStream): Tree = {
    val reader = new InputStreamReader(in)
    lexer.parse(lexer.tokens, reader) match {
      case lexer.Success(tokens, _) =>
        val reader = new parser.DecafReader(tokens)
        parser.topLevel(reader) match {
          case parser.Success(result, _) => result
          case f: parser.NoSuccess =>
            issue(new SyntaxError(f.next.pos))
            logger.info(s"${ f.next.pos }:${ f.msg }:\n${ f.next.pos.longString }")
            TopLevel(Nil)
        }
      case f: lexer.NoSuccess =>
        issue(new SyntaxError(f.next.pos))
        logger.info(s"${ f.next.pos }:${ f.msg }:\n${ f.next.pos.longString }")
        TopLevel(Nil)
    }
  }

  override def post(tree: Tree)(implicit config: Config): Unit = {
    implicit val printer = new IndentPrinter
    //    PA1Tree.pretty(tree)
    PrettyTree.pretty(tree)(printer, PrettyTree.PrettyConfig(showPos = true))
    if (config.target == Config.Target.PA1) {
      config.outputStream.print(printer.toString)
    }
  }
}