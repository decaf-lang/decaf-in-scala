package decaf.typecheck

import decaf.annot.ScopeImplicit._
import decaf.annot.SymbolImplicit._
import decaf.annot.TypeImplicit._
import decaf.annot._
import decaf.driver.{Config, Phase}
import decaf.error._
import decaf.printing.{IndentPrinter, PrettyScope}
import decaf.tree.SyntaxTree._
import decaf.tree.TreeNode._
import decaf.tree.{NamedTree => Named, TypedTree => Typed}

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
class Typer extends Phase[Tree, Typed.Tree]("typer") with Namer {

  /**
    * Entry.
    *
    * @param in a syntax tree
    * @return a typed tree
    */
  override def transform(in: Tree): Typed.Tree = {
    val tree = resolveTree(in)
    val global = new ScopeContext(tree.scope)
    val checkedClasses = tree.classes.map {
      case clazz @ Named.ClassDef(id, parent, fields) =>
        val symbol = clazz.symbol
        val ctx = global.open(symbol.scope)
        val checkedFields = fields.map {
          case v @ Named.VarDef(typeLit, id) => Typed.VarDef(typeLit, id)(v.symbol).setPos(v.pos)
          case f @ Named.MethodDef(isStatic, returnType, id, params, body) =>
            val localCtx = ctx.open(f.symbol.scope)
            val checkedBody = checkStmt(body)(State(), localCtx)
            // FIXME check every path is returned, if its return type is not void
            Typed.MethodDef(isStatic, returnType, id, params, checkedBody)(f.symbol).setPos(f.pos)
        }
        Typed.ClassDef(id, parent, checkedFields)(symbol).setPos(clazz.pos)
    }

    Typed.TopLevel(checkedClasses)(tree.scope).setPos(tree.pos)
  }

  /**
    * After type checking succeeds, pretty print scopes if necessary.
    *
    * @param tree   the typed tree
    * @param config the compiler configuration
    */
  override def post(tree: Typed.Tree)(implicit config: Config): Unit = {
    if (config.target == Config.Target.PA2) {
      implicit val printer: IndentPrinter = new IndentPrinter
      PrettyScope.pretty(tree.scope)
      config.outputStream.print(printer.toString)
    }
  }

  /**
    * Wrap two flags.
    *
    * @param insideLoop    are we inside a loop body?
    * @param directInBlock are we directly in a block, say the level one out is a block?
    *                      TODO this param may not need if we change the scope pretty print rule
    */
  case class State(insideLoop: Boolean = false, directInBlock: Boolean = true) {
    def setInLoop(): State = State(true, false)

    def setNotInBlock(): State = State(insideLoop, false)
  }

  def checkStmt(stmt: Stmt)(implicit state: State, ctx: ScopeContext): Typed.Stmt = {
    // NOTE: as a convention, we always return a skip if the statement is ill-typed.
    val err = Typed.Skip()

    val checked = stmt match {
      case varDef @ LocalVarDef(typeLit, id) =>
        ctx.findConflict(id) match {
          case Some(earlier) =>
            issue(new DeclConflictError(id, earlier.pos, stmt.pos))
            err
          case None =>
            val typedTypeLit = typeTypeLit(typeLit)
            typedTypeLit.typ match {
              case NoType =>
                // NOTE: to avoid flushing a large number of error messages, if we know one error is caused by another,
                // then we shall not report both, but the earlier found one only. In this case, the error of the entire
                // LocalVarDef is caused by the bad typeLit, and thus we don't make further type check.
                err
              case VoidType => issue(new BadVarTypeError(id, stmt.pos)); err
              case t =>
                val symbol = new LocalVarSymbol(varDef, t)
                ctx.declare(symbol)
                Typed.LocalVarDef(typedTypeLit, id)(symbol)
            }
        }

      case Block(stmts) =>
        val scope = ctx.currentScope match {
          case s: FormalScope => s.nestedScope
          case s: LocalScope =>
            val ls = new LocalScope
            if (state.directInBlock) s.nestedScopes += ls
            ls
        }
        val local = ctx.open(scope)
        val ss = stmts.map { checkStmt(_)(state, local) }
        Typed.Block(ss)

      case Assign(lhs, rhs) =>
        val l = typeLValue(lhs)
        val r = typeExpr(rhs)
        l.typ match {
          case NoType => // do nothing
          case _: FunType => issue(new IncompatBinOpError("=", l.typ, r.typ, stmt.pos))
          case t if !(r.typ <= t) => issue(new IncompatBinOpError("=", l.typ, r.typ, stmt.pos))
          case _ => // do nothing
        }
        Typed.Assign(l, r)

      case ExprEval(expr) =>
        val e = typeExpr(expr)
        Typed.ExprEval(e)

      case Skip() => Typed.Skip()

      case If(cond, trueBranch, falseBranch) =>
        val c = checkTestExpr(cond)
        val t = checkStmt(trueBranch)(state.setNotInBlock(), ctx)
        val f = checkStmt(falseBranch)(state.setNotInBlock(), ctx)
        Typed.If(c, t, f)

      case While(cond, body) =>
        val c = checkTestExpr(cond)
        val b = checkStmt(body)(state.setInLoop(), ctx)
        Typed.While(c, b)

      case For(init, cond, update, body) =>
        val i = checkStmt(init)
        val c = checkTestExpr(cond)
        val u = checkStmt(update)
        val b = checkStmt(body)(state.setInLoop(), ctx)
        Typed.For(i, c, u, b)

      case Break() =>
        if (!state.insideLoop) issue(new BreakOutOfLoopError(stmt.pos))
        Typed.Break()

      case Return(expr) =>
        val expected = ctx.currentMethod.returnType
        val e = expr.map(typeExpr)
        val actual = e.map(_.typ).getOrElse(VoidType)
        if (actual.noError && !(actual <= expected)) issue(new BadReturnTypeError(expected, actual, stmt.pos))
        Typed.Return(e)

      case Print(exprs) =>
        val es = exprs.zipWithIndex.map {
          case (expr, i) =>
            val e = typeExpr(expr)
            if (e.typ.noError && !e.typ.isBaseType) issue(new BadPrintArgError(i + 1, e.typ, expr.pos))
            e
        }
        Typed.Print(es)
    }
    checked.setPos(stmt.pos)
  }

  def checkTestExpr(expr: Expr)(implicit ctx: ScopeContext): Typed.Expr = {
    val e = typeExpr(expr)
    if (e.typ !== BoolType) issue(new BadTestExpr(expr.pos))
    e
  }

  implicit val noType: NoType.type = NoType

  def typeExpr(expr: Expr)(implicit ctx: ScopeContext): Typed.Expr = {
    // NOTE: as a convention, we always return a local variable that refers to null when the expression is ill-typed
    // and we cannot figure out a suitable typed expression as guess.
    val err = Typed.LocalVar(null)

    val typed = expr match {
      case e: LValue => typeLValue(e)

      case IntLit(v) => Typed.IntLit(v)(IntType)
      case BoolLit(v) => Typed.BoolLit(v)(BoolType)
      case StringLit(v) => Typed.StringLit(v)(StringType)
      case NullLit() => Typed.NullLit()(NullType)

      case ReadInt() => Typed.ReadInt()(IntType)
      case ReadLine() => Typed.ReadLine()(StringType)

      case UnaryExpr(op, operand) =>
        val e = typeExpr(operand)
        e.typ match {
          case NoType => Typed.UnaryExpr(op, e)
          case t =>
            if (!compatible(op, t)) issue(new IncompatUnOpError(op, t, expr.pos))
            // Even when it doesn't type check, we could make a fair guess based on the operator kind.
            // Let's say the operator is `-`, then one possibly wants an integer as the operand.
            // Once he/she fixes the operand, according to our type inference rule, the whole unary expression
            // must have type int! Thus, we simply _assume_ it has type int, rather than `NoType`.
            Typed.UnaryExpr(op, e)(resultTypeOf(op))
        }

      case BinaryExpr(op, lhs, rhs) =>
        val l = typeExpr(lhs)
        val r = typeExpr(rhs)
        (l.typ, r.typ) match {
          case (_, NoType) | (NoType, _) => Typed.BinaryExpr(op, l, r)
          case (lt, rt) =>
            if (!compatible(op, lt, rt)) issue(new IncompatBinOpError(op, lt, rt, expr.pos))
            Typed.BinaryExpr(op, l, r)(resultTypeOf(op)) // make a fair guess
        }

      case NewArray(elemType, length) =>
        val t = typeTypeLit(elemType)
        val l = typeExpr(length)
        if (t.typ.isVoidType) issue(new BadArrElementError(elemType.pos))
        if (l.typ !== IntType) issue(new BadNewArrayLength(length.pos))
        Typed.NewArray(t, l)(ArrayType(t.typ)) // make a fair guess

      case NewClass(id) =>
        ctx.lookupClass(id) match {
          case Some(clazz) => Typed.NewClass(clazz)(clazz.typ)
          case None => issue(new ClassNotFoundError(id, expr.pos)); err
        }

      case This() =>
        if (ctx.currentMethod.isStatic) issue(new ThisInStaticFuncError(expr.pos))
        Typed.This()(ctx.currentClass.typ) // make a fair guess

      case call @ Call(Some(VarSel(None, id)), method, _) if ctx.containsClass(id) =>
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

      case call @ Call(receiver, method, args) =>
        val r = receiver.map(typeExpr)
        r.map(_.typ).getOrElse(ctx.currentClass.typ) match {
          case NoType => err
          case _: ArrayType if method.name == "length" => // Special case: array.length()
            assert(r.isDefined)
            if (args.nonEmpty) issue(new BadLengthArgError(args.length, expr.pos))
            Typed.ArrayLen(r.get)(IntType)
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

      case ClassTest(obj, clazz) =>
        val o = typeExpr(obj)
        if (!o.typ.isClassType) issue(new NotClassError(o.typ, expr.pos))
        ctx.lookupClass(clazz) match {
          case Some(c) => Typed.ClassTest(o, c)(BoolType)
          case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); err
        }

      case ClassCast(obj, clazz) =>
        val o = typeExpr(obj)
        if (!o.typ.isClassType) issue(new NotClassError(o.typ, o.pos))
        ctx.lookupClass(clazz) match {
          case Some(c) => Typed.ClassCast(o, c)(c.typ)
          case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); err
        }
    }
    typed.setPos(expr.pos)
  }

  def typeCall(call: Call, receiver: Option[Typed.Expr], method: MethodSymbol)
              (implicit ctx: ScopeContext): Typed.Expr = {
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

    if (method.isStatic) Typed.StaticCall(method, as)(method.returnType)
    else Typed.MemberCall(receiver.getOrElse(Typed.This()), method, as)(method.returnType)
  }

  def typeLValue(expr: LValue)(implicit ctx: ScopeContext): Typed.LValue = {
    val err = Typed.LocalVar(null)

    val typed = expr match {
      // Variable, which should be complicated since a legal variable could refer to a local var,
      // a visible member var (, and a class name).
      case VarSel(None, id) =>
        ctx.lookup(id) match {
          case Some(sym) => sym match {
            case v: LocalVarSymbol => Typed.LocalVar(v)(v.typ)
            case v: MemberVarSymbol =>
              if (ctx.currentMethod.isStatic) // member vars cannot be accessed in a static method
                issue(new RefNonStaticError(id, ctx.currentMethod.name, expr.pos))
              Typed.MemberVar(Typed.This(), v)(v.typ)
            // TODO report error: method and types are not allowed here
            case _ => issue(new UndeclVarError(id, expr.pos)); err
          }
          case None => issue(new UndeclVarError(id, expr.pos)); err
        }

      case VarSel(Some(VarSel(None, id)), f) if ctx.containsClass(id) =>
        // special case like MyClass.foo: report error cannot access field 'foo' from 'class : MyClass'
        issue(new NotClassFieldError(f, ctx.getClass(id).typ, expr.pos))
        err

      case VarSel(Some(receiver), id) =>
        val r = typeExpr(receiver)
        r.typ match {
          case NoType => err
          case t @ ClassType(c, _) =>
            ctx.getClass(c).scope.lookup(id) match {
              case Some(sym) => sym match {
                case v: MemberVarSymbol =>
                  if (!(ctx.currentClass.typ <= t)) // member vars are protected
                    issue(new FieldNotAccessError(id, t, expr.pos))
                  Typed.MemberVar(r, v)(v.typ)
                // case m: MethodSymbol => TODO report error: method and types are not allowed here
                case _ => issue(new FieldNotFoundError(id, t, expr.pos)); err
              }
              case None => issue(new FieldNotFoundError(id, t, expr.pos)); err
            }
          case t => issue(new NotClassFieldError(id, t, expr.pos)); err
        }

      case IndexSel(array, index) =>
        val a = typeExpr(array)
        val i = typeExpr(index)
        val typ = a.typ match {
          case ArrayType(elemType) =>
            if (i.typ !== IntType) issue(new SubNotIntError(expr.pos))
            elemType // make a fair guess
          case _ => issue(new NotArrayError(array.pos)); NoType
        }
        Typed.IndexSel(a, i)(typ)
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