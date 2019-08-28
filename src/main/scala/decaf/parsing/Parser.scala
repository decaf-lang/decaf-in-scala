package decaf.parsing

import java.io.InputStream

import decaf.driver.{Config, Phase}
import decaf.error._
import decaf.parsing.Util._
import decaf.parsing.antlr.{DecafParser, DecafParserBaseVisitor}
import decaf.printing.{IndentPrinter, PA1Tree}
import decaf.tree.SyntaxTree._
import decaf.tree.TreeNode
import decaf.tree.TreeNode.Id
import org.antlr.v4.runtime._

import scala.jdk.CollectionConverters._

/**
  * Decaf parser, corresponds to the "parser" phase.
  */
class Parser extends Phase[InputStream, Tree]("parser") {

  /**
    * Entry.
    *
    * @param in the input stream
    * @return the syntax tree
    */
  override def transform(in: InputStream): Tree = {
    val stream = CharStreams.fromStream(in)
    val lexer = new decaf.parsing.Lexer(stream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new DecafParser(tokens)
    parser.addErrorListener(ErrorListener)

    try {
      val topLevel = parser.topLevel
      return TopLevelVisitor.visit(topLevel) // the visitor will return the syntax tree
    } catch {
      case error: Error => issue(error)
    }

    TopLevel(Nil) // when the parsing is interrupted by a fatal error, return empty tree
  }

  /**
    * After parsing succeeds, pretty print the tree if necessary.
    *
    * @param tree   the syntax tree
    * @param config the compiler configuration
    */
  override def post(tree: Tree)(implicit config: Config): Unit = {
    if (config.target == Config.Target.PA1) { // pretty only when the target is PA1
      implicit val printer = new IndentPrinter
      PA1Tree.pretty(tree)
      //    PrettyTree.pretty(tree)(printer, PrettyTree.PrettyConfig(showPos = true))
      config.outputStream.print(printer.toString)
    }
  }

  /**
    * Our own error listener that can append parsing errors to our error issuer. We have to do this because Antlr
    * has his own error recovery strategy -- but we don't want that!
    */
  object ErrorListener extends BaseErrorListener {
    override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Any, lineNumber: Int,
                             charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
      issue(new SyntaxError(msg, new Pos {
        override def line: Int = lineNumber

        override def column: Int = charPositionInLine + 1

        override protected def lineContents: String = ???
      }))
    }
  }

  // --------------------------------------------------------------------------------------------------------------
  // The following methods do the actual parsing stuff. Strictly speaking, it is the Antlr-generated code that
  // parses the decaf source. What we need to do is to simply translate the parsing tree yielded by Antlr into
  // the SyntaxTree defined by ourselves. In short, a tree transformation.
  //
  // The entire parsing process:
  // 1. Antlr-generated parser is invoked and parses the input source. Errors are captured by the above listener.
  // 2. If a fatal error (unrecognized character) occurs, report that and exit. Other errors are appended but they
  //    will not interrupt the process.
  // 3. Finally, call the visitors below and we either obtain a syntax tree or fail with errors.
  //
  // Instead of doing the ugly nested instanceOf check, like this:
  //
  // if (node.isInstanceOf[TopLevel]) {
  //   // do transformation
  // } else if (node.isInstanceOf[ClassDef]) {
  //   // do transformation
  // } else if ...
  //
  // We stick to Java's visitor pattern -- also suggested by Antlr. Although a visitor returns a result of type T,
  // but all methods in the visitor must have the same T. Apparently, this doesn't fit our situation. The syntax
  // tree nodes are heterogeneous -- ClassDef, Field, Stmt, Expr, TypeLit, etc. Thus, a solution is to create
  // separate visitors for each of them, and call the visitor we needed. For example, to create a While node,
  // we first call the ExprVisitor to visit the condition, which gives us an expression `c`. Then, we call the
  // StmtVisitor to visit the loop body, which gives us an expression `s`. Finally, call `While(c, s)` construct
  // the tree node.
  //
  // Besides, remember to set the position, either by `positioned`, which regard the start location of the first
  // token matched in the rule as the position, set by hand if this is not the case.
  // --------------------------------------------------------------------------------------------------------------

  object TopLevelVisitor extends DecafParserBaseVisitor[TopLevel] {
    override def visitTopLevel(ctx: DecafParser.TopLevelContext): TopLevel = positioned(ctx) {
      val classes = ctx.classDef.asScala.toList.map(_.accept(ClassDefVisitor))
      TopLevel(classes)
    }
  }

  object ClassDefVisitor extends DecafParserBaseVisitor[ClassDef] {
    override def visitClassDef(ctx: DecafParser.ClassDefContext): ClassDef = positioned(ctx) {
      val id = ctx.id.accept(IdVisitor)
      // NOTE: if an optional symbol (like extendsClause) is undefined, its corresponding field is null.
      val parent = if (ctx.extendsClause != null) Some(ctx.extendsClause.id.accept(IdVisitor)) else None
      val fields = ctx.field.asScala.toList.map(_.accept(FieldVisitor))
      ClassDef(id, parent, fields)
    }
  }

  object FieldVisitor extends DecafParserBaseVisitor[Field] {
    override def visitVarDef(ctx: DecafParser.VarDefContext): Field = {
      val typ = ctx.`var`.`type`.accept(TypeLitVisitor)
      val id = ctx.`var`.id.accept(IdVisitor)
      VarDef(typ, id).setPos(id.pos)
    }

    override def visitMethodDef(ctx: DecafParser.MethodDefContext): Field = {
      val returnType = ctx.`type`.accept(TypeLitVisitor)
      val id = ctx.id.accept(IdVisitor)
      val params =
        if (ctx.varList == null) Nil else ctx.varList.`var`.asScala.toList.map(_.accept(LocalVarDefVisitor))
      val body = ctx.stmtBlock.accept(StmtVisitor)
      MethodDef(ctx.STATIC != null, returnType, id, params, body).setPos(id.pos)
    }
  }

  object TypeLitVisitor extends DecafParserBaseVisitor[TypeLit] {
    override def visitIntType(ctx: DecafParser.IntTypeContext): TypeLit = positioned(ctx) { TInt() }

    override def visitBoolType(ctx: DecafParser.BoolTypeContext): TypeLit = positioned(ctx) { TBool() }

    override def visitStringType(ctx: DecafParser.StringTypeContext): TypeLit = positioned(ctx) { TString() }

    override def visitVoidType(ctx: DecafParser.VoidTypeContext): TypeLit = positioned(ctx) { TVoid() }

    override def visitClassType(ctx: DecafParser.ClassTypeContext): TypeLit = positioned(ctx) {
      TClass(ctx.id.accept(IdVisitor))
    }

    override def visitArrayType(ctx: DecafParser.ArrayTypeContext): TypeLit = positioned(ctx) {
      TArray(ctx.elemType.accept(this))
    }
  }

  object StmtVisitor extends DecafParserBaseVisitor[Stmt] {
    override def visitLocalVarDef(ctx: DecafParser.LocalVarDefContext): Stmt =
      ctx.varDef.`var`.accept(LocalVarDefVisitor)

    override def visitStmtBlock(ctx: DecafParser.StmtBlockContext): Stmt = positioned(ctx) {
      val stmts = ctx.stmt.asScala.toList.map(_.accept(this))
      Block(stmts)
    }

    override def visitSimpleStmt(ctx: DecafParser.SimpleStmtContext): Stmt = ctx.simple.accept(SimpleStmtVisitor)

    override def visitIf(ctx: DecafParser.IfContext): Stmt = positioned(ctx) {
      val cond = ctx.cond.accept(ExprVisitor)
      val trueBranch = ctx.trueBranch.accept(this)
      // NOTE: if the false branch is not given, regard it as skip.
      val falseBranch = if (ctx.falseBranch == null) Skip() else ctx.falseBranch.accept(this)
      If(cond, trueBranch, falseBranch)
    }

    override def visitWhile(ctx: DecafParser.WhileContext): Stmt = positioned(ctx) {
      val cond = ctx.cond.accept(ExprVisitor)
      val body = ctx.body.accept(this)
      While(cond, body)
    }

    override def visitFor(ctx: DecafParser.ForContext): Stmt = positioned(ctx) {
      val init = ctx.init.accept(SimpleStmtVisitor)
      val cond = ctx.cond.accept(ExprVisitor)
      val update = ctx.update.accept(SimpleStmtVisitor)
      val body = ctx.body.accept(this)
      For(init, cond, update, body)
    }

    override def visitBreak(ctx: DecafParser.BreakContext): Stmt = positioned(ctx) { Break() }

    override def visitReturn(ctx: DecafParser.ReturnContext): Stmt = positioned(ctx) {
      val expr = if (ctx.expr != null) Some(ctx.expr.accept(ExprVisitor)) else None
      Return(expr)
    }

    override def visitPrint(ctx: DecafParser.PrintContext): Stmt = positioned(ctx) {
      Print(ctx.exprList.accept(ExprListVisitor))
    }
  }

  object LocalVarDefVisitor extends DecafParserBaseVisitor[LocalVarDef] {
    override def visitVar(ctx: DecafParser.VarContext): LocalVarDef = {
      val typ = ctx.`type`.accept(TypeLitVisitor)
      val id = ctx.id.accept(IdVisitor)
      LocalVarDef(typ, id).setPos(id.pos)
    }
  }

  object SimpleStmtVisitor extends DecafParserBaseVisitor[SimpleStmt] {
    override def visitAssign(ctx: DecafParser.AssignContext): SimpleStmt = {
      val lhs = ctx.lValue.accept(LValueVisitor)
      val rhs = ctx.expr.accept(ExprVisitor)
      Assign(lhs, rhs).setPos(getPos(ctx.ASSIGN.getSymbol))
    }

    override def visitEval(ctx: DecafParser.EvalContext): SimpleStmt = {
      val receiver = if (ctx.expr != null) Some(ctx.expr.accept(ExprVisitor)) else None
      val id = ctx.id.accept(IdVisitor)
      val args = ctx.exprList.accept(ExprListVisitor)
      Call(receiver, id, args).setPos(id.pos).asExprEval
    }

    override def visitSkip(ctx: DecafParser.SkipContext): SimpleStmt = positioned(ctx) { Skip() }
  }

  object LValueVisitor extends DecafParserBaseVisitor[LValue] {
    override def visitLValueVar(ctx: DecafParser.LValueVarContext): LValue = {
      val receiver = if (ctx.expr != null) Some(ctx.expr.accept(ExprVisitor)) else None
      val id = ctx.id.accept(IdVisitor)
      VarSel(receiver, id).setPos(id.pos)
    }

    override def visitLValueIndex(ctx: DecafParser.LValueIndexContext): LValue = positioned(ctx) {
      val array = ctx.array.accept(ExprVisitor)
      val index = ctx.index.accept(ExprVisitor)
      IndexSel(array, index)
    }
  }

  object ExprVisitor extends DecafParserBaseVisitor[Expr] {

    override def visitLiteral(ctx: DecafParser.LiteralContext): Expr = ctx.lit.accept(this)

    override def visitIntLit(ctx: DecafParser.IntLitContext): Expr = positioned(ctx) {
      val literal = ctx.getText
      var value = -1
      try {
        value = literal.toInt
      } catch {
        case _: NumberFormatException => // not a valid 32-bit integer
          issue(new IntTooLargeError(literal, getPos(ctx.INT_LIT.getSymbol)))
      }

      IntLit(value)
    }

    override def visitBoolLit(ctx: DecafParser.BoolLitContext): Expr = positioned(ctx) {
      BoolLit(ctx.getText.toBoolean)
    }

    override def visitStringLit(ctx: DecafParser.StringLitContext): Expr = positioned(ctx) {
      val buffer = new StringBuilder
      val pos = getPos(ctx.OPEN_STRING.getSymbol)
      ctx.stringChar.asScala.foreach {
        case node if node.ERROR_NEWLINE != null => // handle new line in string
          issue(new NewlineInStrError(quote(buffer.toString), pos))
        case node => buffer ++= node.getText
      }

      if (ctx.UNTERM_STRING != null) { // handle unterminated string
        issue(new UntermStrError(quote(buffer.toString), pos))
      }

      StringLit(buffer.toString)
    }

    override def visitNullLit(ctx: DecafParser.NullLitContext): Expr = positioned(ctx) { NullLit() }

    override def visitThis(ctx: DecafParser.ThisContext): Expr = positioned(ctx) { This() }

    override def visitParen(ctx: DecafParser.ParenContext): Expr = positioned(ctx) { ctx.expr.accept(this) }

    override def visitClassCast(ctx: DecafParser.ClassCastContext): Expr = positioned(ctx) {
      val obj = ctx.expr.accept(this)
      val clazz = ctx.id.accept(IdVisitor)
      ClassCast(obj, clazz)
    }

    override def visitReadInt(ctx: DecafParser.ReadIntContext): Expr = positioned(ctx) { ReadInt() }

    override def visitReadLine(ctx: DecafParser.ReadLineContext): Expr = positioned(ctx) { ReadLine() }

    override def visitNewClass(ctx: DecafParser.NewClassContext): Expr = positioned(ctx) {
      NewClass(ctx.id.accept(IdVisitor))
    }

    override def visitNewArray(ctx: DecafParser.NewArrayContext): Expr = positioned(ctx) {
      val elemType = ctx.elemType.accept(TypeLitVisitor)
      val length = ctx.expr.accept(this)
      NewArray(elemType, length)
    }

    override def visitClassTest(ctx: DecafParser.ClassTestContext): Expr = positioned(ctx) {
      val obj = ctx.expr.accept(this)
      val clazz = ctx.id.accept(IdVisitor)
      ClassTest(obj, clazz)
    }

    override def visitSinglePath(ctx: DecafParser.SinglePathContext): Expr = {
      ctx.varSelOrCall.accept(this)
    }

    override def visitVarSelOrCall(ctx: DecafParser.VarSelOrCallContext): Expr = {
      val id = ctx.id.accept(IdVisitor)
      if (ctx.exprList == null) VarSel(None, id).setPos(id.pos)
      else Call(None, id, ctx.exprList.accept(ExprListVisitor)).setPos(id.pos)
    }

    override def visitPath(ctx: DecafParser.PathContext): Expr = {
      val receiver = ctx.expr.accept(this)
      ctx.varSelOrCall.accept(this) match {
        case v: VarSel => v.withReceiver(receiver)
        case c: Call => c.withReceiver(receiver)
      }
    }

    override def visitIndexSel(ctx: DecafParser.IndexSelContext): Expr = positioned(ctx) {
      val array = ctx.array.accept(this)
      val index = ctx.index.accept(this)
      IndexSel(array, index)
    }

    override def visitUnary(ctx: DecafParser.UnaryContext): Expr = positioned(ctx) {
      val op = ctx.prefix.getText match {
        case "-" => TreeNode.NEG
        case "!" => TreeNode.NOT
      }
      val expr = ctx.expr.accept(this)
      UnaryExpr(op, expr)
    }

    override def visitBinary(ctx: DecafParser.BinaryContext): Expr = {
      val op = ctx.infix.getText match {
        case "*" => TreeNode.MUL
        case "/" => TreeNode.DIV
        case "%" => TreeNode.MOD
        case "+" => TreeNode.ADD
        case "-" => TreeNode.SUB
        case "<=" => TreeNode.LE
        case "<" => TreeNode.LT
        case ">=" => TreeNode.GE
        case ">" => TreeNode.GT
        case "==" => TreeNode.EQ
        case "!=" => TreeNode.NE
        case "&&" => TreeNode.AND
        case "||" => TreeNode.OR
      }
      val lhs = ctx.lhs.accept(this)
      val rhs = ctx.rhs.accept(this)
      BinaryExpr(op, lhs, rhs).setPos(getPos(ctx.infix))
    }
  }

  object ExprListVisitor extends DecafParserBaseVisitor[List[Expr]] {
    override def visitExprList(ctx: DecafParser.ExprListContext): List[Expr] = {
      if (ctx.expr == null) Nil
      else ctx.expr.asScala.toList.map(_.accept(ExprVisitor))
    }
  }

  object IdVisitor extends DecafParserBaseVisitor[Id] {
    override def visitId(ctx: DecafParser.IdContext): Id = positioned(ctx) { Id(ctx.ID.getText) }
  }

}
