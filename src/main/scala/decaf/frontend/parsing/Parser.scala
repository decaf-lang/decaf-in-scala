package decaf.frontend.parsing

import java.io.InputStream

import decaf.driver.error._
import decaf.driver.{Config, Phase}
import decaf.frontend.parsing.Util._
import decaf.frontend.parsing.antlr.{DecafParser, DecafParserBaseVisitor}
import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode
import decaf.frontend.tree.TreeNode.{Id, Modifiers}
import decaf.lowlevel.log.IndentPrinter
import decaf.printing.PrettyTree
import decaf.util.Conversions._
import org.antlr.v4.runtime._

/**
  * The parser phase: parse a Decaf source and build an abstract syntax tree.
  * Antlr specification file: `src/main/antlr4/DecafParser.g4`.
  *
  * ===Overview===
  * Strictly speaking, it is NOT this class, but the Antlr-generated code that parses the Decaf source. What we need is
  * to simply translate the parsing tree yielded by Antlr into a [[decaf.frontend.tree.SyntaxTree]] defined by us.
  * In short, a tree transformation.
  *
  * ===Implementation===
  * The entire parsing process:
  *
  *   1. Antlr-generated parser is invoked and parses the input source. Errors are captured by the above listener.
  *   1. If a fatal error (unrecognized character) occurs, report that and exit. Other errors are appended but they
  * will not interrupt the process.
  *   1. Finally, call the visitors below and we either obtain a syntax tree or fail with errors.
  *
  * Instead of doing the ugly nested instance-of check, like:
  * {{{
  * if (node.isInstanceOf[TopLevel]) {
  *   // do transformation
  * } else if (node.isInstanceOf[ClassDef]) {
  *   // do transformation
  * } else if ...
  * }}}
  *
  * We stick to Java's visitor pattern -- also suggested by Antlr. Although a visitor returns a result of type `T`,
  * but all methods in the visitor must have the same `T`. Apparently, this doesn't fit our situation. The syntax
  * tree nodes are ''heterogeneous'' -- ClassDef, Field, Stmt, Expr, TypeLit, etc. Thus, a solution is to create
  * separate visitors for each of them, and call the visitor we needed. For example, to create a `While` node,
  * we first call the `ExprVisitor` to visit the condition, which gives us an expression `c`. Then, we call the
  * `StmtVisitor` to visit the loop body, which gives us an expression `s`. Finally, calling `While(c, s)` gives us
  * the tree node.
  *
  * Besides, remember to set the position, either by `positioned`, which regard the start location of the first
  * token matched in the rule as the position, or set by hand if this is not the case.
  */
class Parser(implicit config: Config) extends Phase[InputStream, Tree]("parser", config) {

  /**
    * Transformer entry.
    *
    * @param in input stream
    * @return abstract syntax tree
    */
  override def transform(in: InputStream): Tree = {
    val stream = CharStreams.fromStream(in)
    val lexer = new decaf.frontend.parsing.Lexer(stream, this)
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
    * @param tree syntax tree
    */
  override def onSucceed(tree: Tree): Unit = {
    if (config.target == Config.Target.PA1) { // pretty only when the target is PA1
      val printer = new PrettyTree(new IndentPrinter(config.output))
      printer.pretty(tree)
      printer.flush()
    }
  }

  /**
    * Our own error listener that can append parsing errors to our error issuer.
    *
    * We have to do this because Antlr has his own error recovery strategy -- but we don't want that!
    */
  object ErrorListener extends BaseErrorListener {

    override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Any, lineNumber: Int,
                             charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
      throw new SyntaxError(msg, new Pos(lineNumber, charPositionInLine + 1))
    }
  }

  // --------------------------------------------------------------------------------------------------------------
  // The following are our favorite visitors!
  // --------------------------------------------------------------------------------------------------------------

  object TopLevelVisitor extends DecafParserBaseVisitor[TopLevel] {

    override def visitTopLevel(ctx: DecafParser.TopLevelContext): TopLevel = {
      val classes = ctx.classDef.map(_.accept(ClassDefVisitor))
      TopLevel(classes).setPos(classes.head.pos)
    }
  }

  object ClassDefVisitor extends DecafParserBaseVisitor[ClassDef] {

    override def visitClassDef(ctx: DecafParser.ClassDefContext): ClassDef = {
      val id = ctx.id.accept(IdVisitor)
      // NOTE: if an optional symbol (like extendsClause) is undefined, its corresponding field is null.
      val parent = if (ctx.extendsClause != null) Some(ctx.extendsClause.id.accept(IdVisitor)) else None
      val fields = ctx.field.map(_.accept(FieldVisitor))
      ClassDef(id, parent, fields).setPos(getPos(ctx.CLASS.getSymbol))
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
        if (ctx.varList == null) Nil else ctx.varList.`var`.map(_.accept(VarVisitor))
      val body = ctx.stmtBlock.accept(StmtVisitor)
      val mod =
        if (ctx.STATIC == null) {
          new Modifiers
        } else {
          new Modifiers(Modifiers.STATIC, getPos(ctx.STATIC.getSymbol))
        }
      MethodDef(mod, id, returnType, params, body).setPos(id.pos)
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

    override def visitStmtBlock(ctx: DecafParser.StmtBlockContext): Stmt = positioned(ctx) {
      val stmts = ctx.stmt.map(_.accept(this))
      Block(stmts)
    }

    override def visitLocalVarDef(ctx: DecafParser.LocalVarDefContext): Stmt = {
      val theVar = ctx.`var`.accept(VarVisitor)
      if (ctx.expr == null) {
        theVar
      } else {
        val init = ctx.expr.accept(ExprVisitor)
        val ret = LocalVarDef(theVar.typeLit, theVar.id, Some(init), getPos(ctx.ASSIGN.getSymbol)).setPos(theVar.pos)
        ret
      }
    }

    override def visitAssign(ctx: DecafParser.AssignContext): Stmt = {
      val lhs = ctx.lValue.accept(LValueVisitor)
      val rhs = ctx.expr.accept(ExprVisitor)
      Assign(lhs, rhs).setPos(getPos(ctx.ASSIGN.getSymbol))
    }

    override def visitEval(ctx: DecafParser.EvalContext): Stmt = {
      val expr = ctx.expr.accept(ExprVisitor)
      ExprEval(expr).setPos(expr.pos)
    }

    override def visitSkip(ctx: DecafParser.SkipContext): Stmt = positioned(ctx) { Skip() }

    override def visitSimpleStmt(ctx: DecafParser.SimpleStmtContext): Stmt = {
      ctx.simple.accept(this) match {
        case s: Skip => s.setPos(getPos(ctx.SEMI.getSymbol))
        case other => other
      }
    }

    override def visitIf(ctx: DecafParser.IfContext): Stmt = positioned(ctx) {
      val cond = ctx.cond.accept(ExprVisitor)
      val trueBranch = ctx.trueBranch.accept(this)
      // NOTE: if the false branch is not given, regard it as skip.
      val falseBranch = if (ctx.falseBranch == null) None else Some(blocked(ctx.falseBranch.accept(this)))
      If(cond, trueBranch, falseBranch)
    }

    override def visitWhile(ctx: DecafParser.WhileContext): Stmt = positioned(ctx) {
      val cond = ctx.cond.accept(ExprVisitor)
      val body = ctx.body.accept(this)
      While(cond, body)
    }

    override def visitFor(ctx: DecafParser.ForContext): Stmt = positioned(ctx) {
      val init = ctx.init.accept(StmtVisitor) match {
        case s: Skip => s.setPos(getPos(ctx.SEMI(0).getSymbol))
        case other => other
      }
      val cond = ctx.cond.accept(ExprVisitor)
      val update = ctx.update.accept(StmtVisitor) match {
        case s: Skip => s.setPos(getPos(ctx.RPAREN.getSymbol))
        case other => other
      }
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

  object VarVisitor extends DecafParserBaseVisitor[LocalVarDef] {

    override def visitVar(ctx: DecafParser.VarContext): LocalVarDef = {
      val typ = ctx.`type`.accept(TypeLitVisitor)
      val id = ctx.id.accept(IdVisitor)
      LocalVarDef(typ, id).setPos(id.pos)
    }
  }

  object LValueVisitor extends DecafParserBaseVisitor[LValue] {

    override def visitLValueVar(ctx: DecafParser.LValueVarContext): LValue = {
      val receiver = if (ctx.expr != null) Some(ctx.expr.accept(ExprVisitor)) else None
      val id = ctx.id.accept(IdVisitor)
      VarSel(receiver, id).setPos(id.pos)
    }

    override def visitLValueIndex(ctx: DecafParser.LValueIndexContext): LValue = {
      val array = ctx.array.accept(ExprVisitor)
      val index = ctx.index.accept(ExprVisitor)
      IndexSel(array, index).setPos(getPos(ctx.LBRACK.getSymbol))
    }
  }

  object ExprVisitor extends DecafParserBaseVisitor[Expr] {

    override def visitLiteral(ctx: DecafParser.LiteralContext): Expr = ctx.lit.accept(this)

    override def visitIntLit(ctx: DecafParser.IntLitContext): Expr = positioned(ctx) {
      IntLit(ctx.getText.toInt)
    }

    override def visitBoolLit(ctx: DecafParser.BoolLitContext): Expr = positioned(ctx) {
      BoolLit(ctx.getText.toBoolean)
    }

    override def visitStringLit(ctx: DecafParser.StringLitContext): Expr =
      StringLit(ctx.CLOSE_STRING.getText).setPos(getPos(ctx.OPEN_STRING.getSymbol))

    override def visitNullLit(ctx: DecafParser.NullLitContext): Expr = positioned(ctx) { NullLit() }

    override def visitThis(ctx: DecafParser.ThisContext): Expr = positioned(ctx) { This() }

    override def visitParen(ctx: DecafParser.ParenContext): Expr = ctx.expr.accept(this)

    override def visitClassCast(ctx: DecafParser.ClassCastContext): Expr = {
      val obj = ctx.expr.accept(this)
      val clazz = ctx.id.accept(IdVisitor)
      ClassCast(obj, clazz).setPos(obj.pos)
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

    override def visitSinglePath(ctx: DecafParser.SinglePathContext): Expr = ctx.varSelOrCall.accept(this)

    override def visitVarSelOrCall(ctx: DecafParser.VarSelOrCallContext): Expr = {
      val id = ctx.id.accept(IdVisitor)
      if (ctx.exprList == null) {
        VarSel(None, id).setPos(id.pos)
      } else {
        Call(None, id, ctx.exprList.accept(ExprListVisitor)).setPos(getPos(ctx.LPAREN.getSymbol))
      }
    }

    override def visitPath(ctx: DecafParser.PathContext): Expr = {
      val receiver = ctx.expr.accept(this)
      ctx.varSelOrCall.accept(this) match {
        case v: VarSel => v.withReceiver(receiver)
        case c: Call => c.withReceiver(receiver)
      }
    }

    override def visitIndexSel(ctx: DecafParser.IndexSelContext): Expr = {
      val array = ctx.array.accept(this)
      val index = ctx.index.accept(this)
      IndexSel(array, index).setPos(getPos(ctx.LBRACK.getSymbol))
    }

    override def visitUnary(ctx: DecafParser.UnaryContext): Expr = positioned(ctx) {
      val op = ctx.prefix.getText match {
        case "-" => TreeNode.NEG
        case "!" => TreeNode.NOT
      }
      val expr = ctx.expr.accept(this)
      Unary(op, expr)
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
      Binary(op, lhs, rhs).setPos(getPos(ctx.infix))
    }
  }

  object ExprListVisitor extends DecafParserBaseVisitor[List[Expr]] {

    override def visitExprList(ctx: DecafParser.ExprListContext): List[Expr] = {
      if (ctx.expr == null) {
        Nil
      } else {
        ctx.expr.map(_.accept(ExprVisitor))
      }
    }
  }

  object IdVisitor extends DecafParserBaseVisitor[Id] {

    override def visitId(ctx: DecafParser.IdContext): Id = positioned(ctx) { Id(ctx.ID.getText) }
  }

}
