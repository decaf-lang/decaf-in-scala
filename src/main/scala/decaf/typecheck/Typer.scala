package decaf.typecheck

import decaf.annot.FlagImplicit._
import decaf.annot.ScopeImplicit._
import decaf.annot.SymbolImplicit._
import decaf.annot.TypeImplicit._
import decaf.annot._
import decaf.driver.{Config, Phase}
import decaf.error._
import decaf.printing.{IndentPrinter, PrettyScope}
import decaf.tree.TreeNode._
import decaf.tree.TypedTree._
import decaf.tree.{SyntaxTree => Syn}

/**
  * Type check. This phase inputs a SyntaxTree and outputs a TypedTree. In detail, we split this in two passes --
  * namer and typer.
  *
  * Why two passes? Note that all defined classes are visible to every other class, which means we can access another
  * class's members, and of course the type that itself represents, e.g.
  * {{{
  *    class A {
  *      class B foo; // access B
  *      void bar() {
  *        foo.baz(); // access baz of B
  *      }
  *    }
  *    class B {
  *      void baz();
  *    }
  * }}}
  *
  * Apparently, classes cannot be resolved in the order they presented in the syntax tree: class A refers to class B,
  * whose definition goes later. To tackle this issue, one possible way is to first scan all classes "roughly"
  * and then step into details of every method definition -- because at that time, signatures of class members are
  * known. That's why we split the type checking phase into two passes: In the namer pass, we only scan class
  * members, while ignoring any method body, because that's enough for us to know what a class looks like.
  * In the typer pass, we then step into every method body and type check every single statement and expression.
  *
  * Typer will NOT be interrupted by type errors. Instead, an ill-typed term will either be filled with a guessed
  * type, if this term could only be of that type in some situations, or we replace it with an error node.
  *
  * By the way, realizing that a context/environment is fairly needed in the entire type checking process, we need to
  * pass it as a parameter all the time. Thanks to the implicits, we save as many characters as we can, if the wanted
  * context is the same as the "current" one.
  *
  * This file only involves the code for typer pass.
  */
class Typer extends Phase[Tree, Tree]("typer") with Util {

  /**
    * Entry.
    *
    * @param tree a typed tree with untyped holes
    * @return a typed tree without holes if succeeds
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
            val checkedBody = checkBlock(body)(formalCtx)
            // Check if the body always returns a value, when the method is non-void
            if (!m.symbol.returnType.isVoidType && checkedBody.no)
              issue(new MissingReturnError(checkedBody.pos))
            MethodDef(mod, id, returnType, params, checkedBody)(m.symbol).setPos(m.pos)
        }
        ClassDef(id, parent, checkedFields)(symbol).setPos(clazz.pos)
    }

    TopLevel(checkedClasses)(tree.scope).setPos(tree.pos)
  }

  /**
    * After type checking succeeds, pretty print scopes if necessary.
    *
    * @param tree   the typed tree
    * @param config the compiler configuration
    */
  override def post(tree: Tree)(implicit config: Config): Unit = {
    if (config.target == Config.Target.PA2) {
      implicit val printer: IndentPrinter = new IndentPrinter
      PrettyScope.pretty(tree.scope)
      config.outputStream.print(printer.toString)
    }
  }

  def checkBlock(block: Block)(implicit ctx: ScopeContext, insideLoop: Boolean = false): Block = {
    val scope = ctx.currentScope match {
      case s: FormalScope => s.nestedScope
      case s: LocalScope =>
        s.nestedScopes += new LocalScope
        s.nestedScopes.last
    }
    val local = ctx.open(scope)
    val ss = block.stmts.map { checkStmt(_)(local, insideLoop) }
    val ret = if (ss.isEmpty) No else ss.last match {
      case c: ControlFlowStmt => c.flag // a block returns a value if its last statement does so
      case _ => No
    }

    Block(ss)(ret).setPos(block.pos)
  }

  def checkStmt(stmt: Stmt)(implicit ctx: ScopeContext, insideLoop: Boolean): Stmt = {
    implicit val noReturn: Flag = No

    val checked = stmt match {
      case block: Block => checkBlock(block)

      case v: LocalVarDef => ctx.declare(v.symbol); v

      case Assign(lhs, rhs) =>
        val l = typeLValue(lhs)
        val r = typeExpr(rhs)
        l.typ match {
          case NoType => // do nothing
          case _: FunType => issue(new IncompatBinOpError("=", l.typ, r.typ, stmt.pos))
          case t if !(r.typ <= t) => issue(new IncompatBinOpError("=", l.typ, r.typ, stmt.pos))
          case _ => // do nothing
        }
        Assign(l, r)

      case ExprEval(expr) =>
        val e = typeExpr(expr)
        ExprEval(e)

      case Skip() => Skip()

      case If(cond, trueBranch, falseBranch) =>
        val c = checkTestExpr(cond)
        val t = checkBlock(trueBranch)
        val f = falseBranch.map(checkBlock)
        // if-stmt returns a value if both branches return
        val ret = if (t.yes && f.isDefined && f.get.yes) Yes else No
        If(c, t, f)(ret)

      case While(cond, body) =>
        val c = checkTestExpr(cond)
        val b = checkBlock(body)(ctx, insideLoop = true)
        While(c, b)

      case For(init, cond, update, body) =>
        val i = checkStmt(init)
        val c = checkTestExpr(cond)
        val u = checkStmt(update)
        val b = checkBlock(body)(ctx, insideLoop = true)
        For(i, c, u, b)

      case Break() =>
        if (!insideLoop) issue(new BreakOutOfLoopError(stmt.pos))
        Break()

      case Return(expr) =>
        val expected = ctx.currentMethod.returnType
        val e = expr match {
          case Some(e1) => Some(typeExpr(e1))
          case None => None
        }
        val actual = e.map(_.typ).getOrElse(VoidType)
        if (actual.noError && !(actual <= expected)) issue(new BadReturnTypeError(expected, actual, stmt.pos))
        Return(e)(if (e.isDefined) Yes else No) // returned if it has an expression

      case Print(exprs) =>
        val es = exprs.zipWithIndex.map {
          case (expr, i) =>
            val e = typeExpr(expr)
            if (e.typ.noError && !e.typ.isBaseType) issue(new BadPrintArgError(i + 1, e.typ, expr.pos))
            e
        }
        Print(es)
    }
    checked.setPos(stmt.pos)
  }

  def checkTestExpr(expr: Syn.Expr)(implicit ctx: ScopeContext): Expr = {
    val e = typeExpr(expr)
    if (e.typ !== BoolType) issue(new BadTestExpr(expr.pos))
    e
  }

  implicit val noType: NoType.type = NoType

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
        if (t.typ.isVoidType) issue(new BadArrElementError(elemType.pos)) // TODO: err
        if (l.typ !== IntType) issue(new BadNewArrayLength(length.pos)) // TODO: if no error
        NewArray(t, l)(ArrayType(t.typ)) // make a fair guess

      case Syn.NewClass(id) =>
        ctx.lookupClass(id) match {
          case Some(clazz) => NewClass(clazz)(clazz.typ)
          case None => issue(new ClassNotFoundError(id, expr.pos)); err
        }

      case Syn.This() =>
        if (ctx.currentMethod.isStatic) issue(new ThisInStaticFuncError(expr.pos))
        This()(ctx.currentClass.typ) // make a fair guess

      case call @ Syn.Call(Some(Syn.VarSel(None, id)), method, _) if ctx.containsClass(id) =>
        // Special case: invoking a static method, like MyClass.foo()
        val clazz = ctx.getClass(id)
        clazz.scope.lookup(method) match {
          case Some(symbol) => symbol match {
            case m: MethodSymbol =>
              if (m.isStatic) typeCall(call, None, m)
              else { issue(new NotClassFieldError(method, clazz.typ, expr.pos)); err }
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
            ctx.getClass(c).scope.lookup(method) match {
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
        ctx.lookupClass(clazz) match {
          case Some(c) => ClassTest(o, c)(BoolType)
          case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); err
        }

      case Syn.ClassCast(obj, clazz) =>
        val o = typeExpr(obj)
        if (!o.typ.isClassType) issue(new NotClassError(o.typ, o.pos))
        ctx.lookupClass(clazz) match {
          case Some(c) => ClassCast(o, c)(c.typ)
          case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); err
        }
    }
    typed.setPos(expr.pos)
  }

  def typeCall(call: Syn.Call, receiver: Option[Expr], method: MethodSymbol)
              (implicit ctx: ScopeContext): Expr = {
    // Cannot call this's member methods in a static method
    if (receiver.isEmpty && ctx.currentMethod.isStatic && !method.isStatic)
      issue(new RefNonStaticError(method.name, ctx.currentMethod.name, call.pos))

    val args = call.args
    if (method.arity != args.length)
      issue(new BadArgCountError(method.name, method.arity, args.length, call.pos))

    val as = (method.typ.params zip args).zipWithIndex.map {
      case ((t, a), i) =>
        val e = typeExpr(a)
        if (e.typ.noError && !(e.typ <= t))
          issue(new BadArgTypeError(i + 1, t, e.typ, a.pos))
        e
    }

    if (method.isStatic) StaticCall(method, as)(method.returnType)
    else MemberCall(receiver.getOrElse(This()), method, as)(method.returnType)
  }

  def typeLValue(expr: Syn.LValue)(implicit ctx: ScopeContext): LValue = {
    val err = UntypedLValue(expr)

    val typed = expr match {
      // Variable, which should be complicated since a legal variable could refer to a local var,
      // a visible member var (, and a class name).
      case Syn.VarSel(None, id) =>
        ctx.lookup(id) match {
          case Some(sym) => sym match {
            case v: LocalVarSymbol => LocalVar(v)(v.typ)
            case v: MemberVarSymbol =>
              if (ctx.currentMethod.isStatic) // member vars cannot be accessed in a static method
                issue(new RefNonStaticError(id, ctx.currentMethod.name, expr.pos))
              MemberVar(This(), v)(v.typ)
            case _ => issue(new UndeclVarError(id, expr.pos)); err
          }
          case None => issue(new UndeclVarError(id, expr.pos)); err
        }

      case Syn.VarSel(Some(Syn.VarSel(None, id)), f) if ctx.containsClass(id) =>
        // special case like MyClass.foo: report error cannot access field 'foo' from 'class : MyClass'
        issue(new NotClassFieldError(f, ctx.getClass(id).typ, expr.pos))
        err

      case Syn.VarSel(Some(receiver), id) =>
        val r = typeExpr(receiver)
        r.typ match {
          case NoType => err
          case t @ ClassType(c, _) =>
            ctx.getClass(c).scope.lookup(id) match {
              case Some(sym) => sym match {
                case v: MemberVarSymbol =>
                  if (!(ctx.currentClass.typ <= t)) // member vars are protected
                    issue(new FieldNotAccessError(id, t, expr.pos))
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

  def compatible(op: Op, operand: Type): Boolean = op match {
    case NEG => operand === IntType // if e : int, then -e : int
    case NOT => operand === BoolType // if e : bool, then !e : bool
  }

  def compatible(op: Op, lhs: Type, rhs: Type): Boolean = op match {
    case _: ArithOp => (lhs === IntType) && (rhs === IntType) // if e1, e2 : int, then e1 + e2 : int
    case _: LogicOp => (lhs === BoolType) && (rhs === BoolType) // if e1, e2 : bool, then e1 && e2 : bool
    case _: EqOp => (lhs <= rhs) || (rhs <= lhs) // if e1 : T1, e2 : T2, T1 <: T2 or T2 <: T1, then e1 == e2 : bool
    case _: CmpOp => (lhs === IntType) && (rhs === IntType) // if e1, e2 : int, then e1 > e2 : bool
  }

  def resultTypeOf(op: Op): Type = op match {
    case NEG => IntType
    case NOT => BoolType
    case _: ArithOp => IntType
    case _: LogicOp | _: EqOp | _: CmpOp => BoolType
  }
}