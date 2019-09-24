package decaf.frontend.typecheck

import decaf.driver.error._
import decaf.driver.{Config, Phase}
import decaf.frontend.annot.ScopeImplicit._
import decaf.frontend.annot.SymbolImplicit._
import decaf.frontend.annot.TypeImplicit._
import decaf.frontend.annot._
import decaf.frontend.parsing.Pos
import decaf.frontend.tree.SyntaxTree.NoAnnot
import decaf.frontend.tree.TreeNode._
import decaf.frontend.tree.TypedTree._
import decaf.frontend.tree.{SyntaxTree => Syn}
import decaf.lowlevel.log.IndentPrinter
import decaf.printing.PrettyScope

/**
  * The typer phase: type check every statement and expression. It starts right after [[Namer]].
  *
  * Typer will NOT be interrupted by any type error. Instead, an ill-typed term will either be filled with a guessed
  * type, if this term could only be of that type in some situations, or we replace it with an error node.
  *
  * @see [[Namer]]
  * @see [[decaf.frontend.annot.Type]]
  */
class Typer(implicit config: Config) extends Phase[Tree, Tree]("typer", config) with Util {

  /**
    * Transformer entry.
    *
    * @param tree a typed tree with untyped holes
    * @return a fully typed (i.e. without holes) tree if succeeds
    */
  override def transform(tree: Tree): Tree = {
    val global = new ScopeContext(tree.scope)
    val checkedClasses = tree.classes.map {
      case clazz @ ClassDef(id, parent, fields) =>
        val symbol = clazz.symbol
        val ctx = global.open(symbol.scope)
        val checkedFields = fields.map {
          case v: VarDef => v
          case m @ MethodDef(mod, id, returnType, params, body) =>
            val formalCtx = ctx.open(m.symbol.scope)
            val (checkedBody, returns) = checkBlock(body)(formalCtx)
            // Check if the body always returns a value, when the method is non-void
            if (!m.symbol.returnType.isVoidType && !returns) {
              issue(new MissingReturnError(checkedBody.pos))
            }
            MethodDef(mod, id, returnType, params, checkedBody)(m.symbol).setPos(m.pos)
        }
        ClassDef(id, parent, checkedFields)(symbol).setPos(clazz.pos)
    }

    TopLevel(checkedClasses)(tree.scope).setPos(tree.pos)
  }

  /**
    * After type checking succeeds, pretty print scopes if necessary.
    *
    * @param tree the typed tree
    */
  override def onSucceed(tree: Tree): Unit = {
    if (config.target == Config.Target.PA2) {
      val printer = new PrettyScope(new IndentPrinter(config.output))
      printer.pretty(tree.scope)
      printer.flush()
    }
  }

  /**
    * Type check a statement block.
    *
    * @param block      statement block
    * @param ctx        scope context
    * @param insideLoop are we inside a loop?
    * @return a pair: the typed block, and a boolean indicating if this block returns a value
    */
  def checkBlock(block: Block)(implicit ctx: ScopeContext, insideLoop: Boolean = false): (Block, Boolean) = {
    val localCtx = ctx.open(block.scope)
    val ss = block.stmts.map { checkStmt(_)(localCtx, insideLoop) }
    val returns = ss.nonEmpty && ss.last._2 // a block returns a value iff its last statement does so

    (Block(ss.map(_._1))(block.scope).setPos(block.pos), returns)
  }

  /**
    * Type check a statement.
    *
    * @param stmt       statement
    * @param ctx        scope context
    * @param insideLoop are we inside a loop?
    * @return a pair: the typed statement, and a boolean indicating if this statement returns a value
    */
  def checkStmt(stmt: Stmt)(implicit ctx: ScopeContext, insideLoop: Boolean): (Stmt, Boolean) = {
    val checked = stmt match {
      case block: Block => checkBlock(block)

      case v: LocalVarDef =>
        v.init match {
          case Some(expr) =>
            // Special: we need to be careful that the initializer may cyclically refer to the declared variable, e.g.
            // var x = x + 1.
            //     ^
            //     before pos
            // So, we must rectify the "before pos" as the position of the declared variable.
            correctBeforePos = Some(v.id.pos)
            val r = typeExpr(expr)
            correctBeforePos = None // recover

            if (!(r.typ <= v.typeLit.typ)) issue(new IncompatBinOpError("=", v.typeLit.typ, r.typ, v.assignPos))
            (LocalVarDef(v.typeLit, v.id, Some(r))(v.symbol), false)
          case None => (v, false)
        }

      case Assign(lhs, rhs) =>
        val l = typeLValue(lhs)
        val r = typeExpr(rhs)
        l.typ match {
          case NoType => // do nothing
          case _: FunType => issue(new IncompatBinOpError("=", l.typ, r.typ, stmt.pos))
          case t if !(r.typ <= t) => issue(new IncompatBinOpError("=", l.typ, r.typ, stmt.pos))
          case _ => // do nothing
        }
        (Assign(l, r), false)

      case ExprEval(expr) =>
        val e = typeExpr(expr)
        (ExprEval(e), false)

      case Skip() => (Skip(), false)

      case If(cond, trueBranch, falseBranch) =>
        val c = checkTestExpr(cond)
        val (t, trueReturns) = checkBlock(trueBranch)
        val f = falseBranch.map(checkBlock)
        // if-stmt returns a value if both branches return
        val returns = trueReturns && f.isDefined && f.get._2
        (If(c, t, f.map(_._1)), returns)

      case While(cond, body) =>
        val c = checkTestExpr(cond)
        val (b, _) = checkBlock(body)(ctx, insideLoop = true)
        (While(c, b), false)

      case For(init, cond, update, body) =>
        // Since `init` and `update` may declare local variables, remember to first open the local scope of `body`.
        val local = ctx.open(body.scope)
        val (i, _) = checkStmt(init)(local, insideLoop)
        val c = checkTestExpr(cond)(local)
        val (u, _) = checkStmt(update)(local, insideLoop)
        val ss = body.stmts.map { checkStmt(_)(local, insideLoop = true) }
        val b = Block(ss.map(_._1))(body.scope)
        (For(i, c, u, b), false)

      case Break() =>
        if (!insideLoop) issue(new BreakOutOfLoopError(stmt.pos))
        (Break(), false)

      case Return(expr) =>
        val expected = ctx.currentMethod.returnType
        val e = expr match {
          case Some(e1) => Some(typeExpr(e1))
          case None => None
        }
        val actual = e.map(_.typ).getOrElse(VoidType)
        if (actual.noError && !(actual <= expected)) issue(new BadReturnTypeError(expected, actual, stmt.pos))
        (Return(e), e.isDefined)

      case Print(exprs) =>
        val es = exprs.zipWithIndex.map {
          case (expr, i) =>
            val e = typeExpr(expr)
            if (e.typ.noError && !e.typ.isBaseType) issue(new BadPrintArgError(i + 1, e.typ, expr.pos))
            e
        }
        (Print(es), false)
    }

    checked match {
      case (s, r) => (s.setPos(stmt.pos), r)
    }
  }

  /**
    * Check if an expression has type bool.
    *
    * @param expr expression
    * @param ctx  scope context
    * @return true if it has type bool
    */
  private def checkTestExpr(expr: Syn.Expr)(implicit ctx: ScopeContext): Expr = {
    val e = typeExpr(expr)
    if (e.typ !== BoolType) issue(new BadTestExpr(expr.pos))
    e
  }

  implicit val noType: NoType.type = NoType

  /**
    * Type check an expression.
    *
    * @param expr expression
    * @param ctx  scope context
    * @return typed expression
    */
  def typeExpr(expr: Syn.Expr)(implicit ctx: ScopeContext): Expr = {
    val err = UntypedExpr(expr)

    val typed = expr match {
      case e: Syn.LValue => typeLValue(e)

      case Syn.IntLit(v) => IntLit(v)(IntType)
      case Syn.BoolLit(v) => BoolLit(v)(BoolType)
      case Syn.StringLit(v) => StringLit(v)(StringType)
      case Syn.NullLit() => NullLit()(NullType)

      case Syn.ReadInt() => ReadInt()(IntType)
      case Syn.ReadLine() => ReadLine()(StringType)

      case Syn.Unary(op, operand) =>
        val e = typeExpr(operand)
        e.typ match {
          case NoType => // avoid nested errors
          case t =>
            if (!compatible(op, t)) issue(new IncompatUnOpError(op, t, expr.pos))
        }
        // Even when it doesn't type check, we could make a fair guess based on the operator kind.
        // Let's say the operator is `-`, then one possibly wants an integer as the operand.
        // Once he/she fixes the operand, according to our type inference rule, the whole unary expression
        // must have type int! Thus, we simply _assume_ it has type int, rather than `NoType`.
        Unary(op, e)(resultTypeOf(op))

      case Syn.Binary(op, lhs, rhs) =>
        val l = typeExpr(lhs)
        val r = typeExpr(rhs)
        (l.typ, r.typ) match {
          case (_, NoType) | (NoType, _) => // avoid nested errors
          case (lt, rt) =>
            if (!compatible(op, lt, rt)) issue(new IncompatBinOpError(op, lt, rt, expr.pos))
        }
        Binary(op, l, r)(resultTypeOf(op)) // make a fair guess

      case Syn.NewArray(elemType, length) =>
        val t = typeTypeLit(elemType)
        val l = typeExpr(length)
        if (t.typ.isVoidType) issue(new BadArrElementError(elemType.pos))
        if (l.typ !== IntType) issue(new BadNewArrayLength(length.pos))
        NewArray(t, l)(ArrayType(t.typ)) // make a fair guess

      case Syn.NewClass(id) =>
        ctx.global.find(id) match {
          case Some(clazz) => NewClass(clazz)(clazz.typ)
          case None => issue(new ClassNotFoundError(id, expr.pos)); err
        }

      case Syn.This() =>
        if (ctx.currentMethod.isStatic) issue(new ThisInStaticFuncError(expr.pos))
        This()(ctx.currentClass.typ) // make a fair guess

      case call @ Syn.Call(Some(Syn.VarSel(None, id)), method, _) if ctx.global.contains(id) =>
        // Special case: invoking a static method, like MyClass.foo()
        val clazz = ctx.global(id)
        clazz.scope.lookup(method) match {
          case Some(symbol) => symbol match {
            case m: MethodSymbol =>
              if (m.isStatic) {
                typeCall(call, None, m)
              } else { issue(new NotClassFieldError(method, clazz.typ, expr.pos)); err }
            case _ => issue(new NotClassMethodError(method, clazz.typ, expr.pos)); err
          }
          case None => issue(new FieldNotFoundError(method, clazz.typ, expr.pos)); err
        }

      case call @ Syn.Call(receiver, method, args) =>
        val r = receiver.map(typeExpr)
        r.map(_.typ).getOrElse(ctx.currentClass.typ) match {
          case NoType => err
          case _: ArrayType if method.name == "length" => // Special case: array.length()
            assert(r.isDefined)
            if (args.nonEmpty) issue(new BadLengthArgError(args.length, expr.pos))
            ArrayLen(r.get)(IntType)
          case t @ ClassType(c, _) =>
            ctx.global(c).scope.lookup(method) match {
              case Some(sym) => sym match {
                case m: MethodSymbol => typeCall(call, r, m)
                case _ => issue(new NotClassMethodError(method, t, expr.pos)); err
              }
              case None => issue(new FieldNotFoundError(method, t, expr.pos)); err
            }
          case t => issue(new NotClassFieldError(method, t, expr.pos)); err
        }

      case Syn.ClassTest(obj, clazz) =>
        val o = typeExpr(obj)
        if (!o.typ.isClassType) issue(new NotClassError(o.typ, expr.pos))
        ctx.global.find(clazz) match {
          case Some(c) => ClassTest(o, c)(BoolType)
          case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); err
        }

      case Syn.ClassCast(obj, clazz) =>
        val o = typeExpr(obj)
        if (!o.typ.isClassType) issue(new NotClassError(o.typ, o.pos))
        ctx.global.find(clazz) match {
          case Some(c) => ClassCast(o, c)(c.typ)
          case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); err
        }
    }
    typed.setPos(expr.pos)
  }

  private def typeCall(call: Syn.Call, receiver: Option[Expr], method: MethodSymbol)
                      (implicit ctx: ScopeContext): Expr = {
    // Cannot call this's member methods in a static method
    if (receiver.isEmpty && ctx.currentMethod.isStatic && !method.isStatic) {
      issue(new RefNonStaticError(method.name, ctx.currentMethod.name, call.pos))
    }
    val args = call.args
    if (method.arity != args.length) {
      issue(new BadArgCountError(method.name, method.arity, args.length, call.pos))
    }
    val as = (method.typ.args zip args).zipWithIndex.map {
      case ((t, a), i) =>
        val e = typeExpr(a)
        if (e.typ.noError && !(e.typ <= t)) {
          issue(new BadArgTypeError(i + 1, t, e.typ, a.pos))
        }
        e
    }

    if (method.isStatic) {
      StaticCall(method, as)(method.returnType)
    } else {
      MemberCall(receiver.getOrElse(This()), method, as)(method.returnType)
    }
  }

  private def typeLValue(expr: Syn.LValue)(implicit ctx: ScopeContext): LValue = {
    val err = UntypedLValue(expr)

    val typed = expr match {
      // Variable, which should be complicated since a legal variable could refer to a local var,
      // a visible member var (, and a class name).
      case Syn.VarSel(None, id) =>
        // Be careful we may be inside the initializer, if so, load the correct "before position".
        ctx.lookupBefore(id, correctBeforePos.getOrElse(expr.pos)) match {
          case Some(sym) => sym match {
            case v: LocalVarSymbol => LocalVar(v)(v.typ)
            case v: MemberVarSymbol =>
              if (ctx.currentMethod.isStatic) // member vars cannot be accessed in a static method
              {
                issue(new RefNonStaticError(id, ctx.currentMethod.name, expr.pos))
              }
              MemberVar(This(), v)(v.typ)
            case _ => issue(new UndeclVarError(id, expr.pos)); err
          }
          case None => issue(new UndeclVarError(id, expr.pos)); err
        }

      case Syn.VarSel(Some(Syn.VarSel(None, id)), f) if ctx.global.contains(id) =>
        // special case like MyClass.foo: report error cannot access field 'foo' from 'class : MyClass'
        issue(new NotClassFieldError(f, ctx.global(id).typ, expr.pos))
        err

      case Syn.VarSel(Some(receiver), id) =>
        val r = typeExpr(receiver)
        r.typ match {
          case NoType => err
          case t @ ClassType(c, _) =>
            ctx.global(c).scope.lookup(id) match {
              case Some(sym) => sym match {
                case v: MemberVarSymbol =>
                  if (!(ctx.currentClass.typ <= t)) // member vars are protected
                  {
                    issue(new FieldNotAccessError(id, t, expr.pos))
                  }
                  MemberVar(r, v)(v.typ)
                case _ => issue(new FieldNotFoundError(id, t, expr.pos)); err
              }
              case None => issue(new FieldNotFoundError(id, t, expr.pos)); err
            }
          case t => issue(new NotClassFieldError(id, t, expr.pos)); err
        }

      case Syn.IndexSel(array, index) =>
        val a = typeExpr(array)
        val i = typeExpr(index)
        val typ = a.typ match {
          case ArrayType(elemType) =>
            if (i.typ !== IntType) issue(new SubNotIntError(expr.pos))
            elemType // make a fair guess
          case _ => issue(new NotArrayError(array.pos)); NoType
        }
        IndexSel(a, i)(typ)
    }
    typed.setPos(expr.pos)
  }

  private var correctBeforePos: Option[Pos] = None

  private def compatible(op: Op, operand: Type): Boolean = op match {
    case NEG => operand === IntType // if e : int, then -e : int
    case NOT => operand === BoolType // if e : bool, then !e : bool
  }

  private def compatible(op: Op, lhs: Type, rhs: Type): Boolean = op match {
    case _: ArithOp => (lhs === IntType) && (rhs === IntType) // if e1, e2 : int, then e1 + e2 : int
    case _: LogicOp => (lhs === BoolType) && (rhs === BoolType) // if e1, e2 : bool, then e1 && e2 : bool
    case _: EqOp => (lhs <= rhs) || (rhs <= lhs) // if e1 : T1, e2 : T2, T1 <: T2 or T2 <: T1, then e1 == e2 : bool
    case _: CmpOp => (lhs === IntType) && (rhs === IntType) // if e1, e2 : int, then e1 > e2 : bool
  }

  private def resultTypeOf(op: Op): Type = op match {
    case NEG => IntType
    case NOT => BoolType
    case _: ArithOp => IntType
    case _: LogicOp | _: EqOp | _: CmpOp => BoolType
  }
}