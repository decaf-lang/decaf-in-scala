package decaf.frontend.typecheck

import decaf.driver.{Config, Phase}
import decaf.error._
import decaf.frontend.annot.ScopedImplicit._
import decaf.frontend.annot.SymbolizedImplicit._
import decaf.frontend.annot.TypedImplicit._
import decaf.frontend.annot._
import decaf.frontend.printing.{IndentPrinter, PrettyScope, PrettyTree}
import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode._
import decaf.frontend.tree.{NamedTree => Named, TypedTree => Typed}

class Typer extends Phase[Named.Tree, Typed.Tree]("typer") with Util {

  override def transform(tree: Named.Tree): Typed.Tree = {
    val global = new ScopeContext(tree.scope)
    val checkedClasses = tree.classes.map {
      case clazz @ Named.ClassDef(id, parent, fields) =>
        val symbol = clazz.symbol
        val ctx = global.open(symbol.scope)
        val checkedFields = fields.map {
          case v @ Named.VarDef(typeLit, id) => Typed.VarDef(typeLit, id)(v.symbol).setPos(v.pos)
          case f @ Named.MethodDef(isStatic, returnType, id, params, body) =>
            implicit val localCtx: ScopeContext = ctx.open(f.symbol.scope)
            val checkedBody = checkStmt(body)
            // TODO check return
            Typed.MethodDef(isStatic, returnType, id, params, checkedBody)(f.symbol).setPos(f.pos)
        }
        Typed.ClassDef(id, parent, checkedFields)(symbol).setPos(clazz.pos)
    }

    Typed.TopLevel(checkedClasses)(tree.scope).setPos(tree.pos)
  }

  def checkStmt(stmt: Stmt)(implicit insideLoop: Boolean = false, ctx: ScopeContext): Typed.Stmt = {
    val checked = stmt match {
      case varDef @ LocalVarDef(typeLit, id) =>
        val r = ctx.findConflict(id.name) match {
          case Some(earlier) =>
            issue(new DeclConflictError(id.name, earlier.pos, varDef.pos))
            None
          case None =>
            val typedTypeLit = typeTypeLit(typeLit)
            typedTypeLit.typ match {
              case NoType => None
              case VoidType => issue(new BadVarTypeError(id.name, varDef.pos)); None
              case t =>
                val symbol = new LocalVarSymbol(varDef, t)
                ctx.declare(symbol)
                Some(Typed.LocalVarDef(typedTypeLit, id)(symbol).setPos(varDef.pos))
            }
        }
        r match {
          case Some(value) => value
          case None => Typed.Skip()
        }

      case Block(stmts) =>
        val scope = ctx.current match {
          case s: FormalScope => s.nestedScope
          case s: LocalScope =>
            s.nestedScopes += new LocalScope
            s.nestedScopes.last
        }
        val local = ctx.open(scope)
        val ss = stmts.map { checkStmt(_)(insideLoop, local) }
        Typed.Block(ss)

      case Assign(lhs, rhs) =>
        val l = typeLValue(lhs)
        val r = typeExpr(rhs)
        l.typ match {
          case NoType =>
          case _: FunType => issue(new IncompatBinOpError(ASS, l.typ, r.typ, stmt.pos))
          case t if !(r.typ <= t) => issue(new IncompatBinOpError(ASS, l.typ, r.typ, stmt.pos))
          case _ =>
        }
        Typed.Assign(l, r)

      case ExprEval(expr) =>
        val e = typeExpr(expr)
        Typed.ExprEval(e)

      case Skip() => Typed.Skip()

      case If(cond, trueBranch, falseBranch) =>
        val c = checkTestExpr(cond)
        val t = checkStmt(trueBranch)
        val f = checkStmt(falseBranch)
        Typed.If(c, t, f)

      case While(cond, body) =>
        val c = checkTestExpr(cond)
        val b = checkStmt(body)(true, ctx)
        Typed.While(c, b)

      case For(init, cond, update, body) =>
        val i = checkStmt(init)
        val c = checkTestExpr(cond)
        val u = checkStmt(update)
        val b = checkStmt(body)(true, ctx)
        Typed.For(i, c, u, b)

      case Break() =>
        if (!insideLoop) issue(new BreakOutOfLoopError(stmt.pos))
        Typed.Break()

      case Return(expr) =>
        val expected = ctx.currentFun.returnType
        val e = expr.map(typeExpr)
        val actual = e.map(_.typ).getOrElse(VoidType)
        if (actual.noError && !(actual <= expected)) issue(new BadReturnTypeError(expected, actual, stmt.pos))
        Typed.Return(e)

      case Print(exprs) =>
        val es = exprs.zipWithIndex.map {
          case (expr, i) =>
            val e = typeExpr(expr)
            if (e.typ.noError && !e.typ.isBaseType) issue(new BadPrintArgError(i, e.typ, expr.pos))
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

  object ASS extends BinaryOp {
    override val str: String = "="
  } // TODO

  implicit val noType = NoType

  def typeExpr(expr: Expr)(implicit ctx: ScopeContext): Typed.Expr = {
    val typed = expr match {
      // Transfer left values to `typeLValue`
      case e: LValue => typeLValue(e)

      // Trivial cases for literals
      case IntLit(v) => Typed.IntLit(v)(IntType)
      case BoolLit(v) => Typed.BoolLit(v)(BoolType)
      case StringLit(v) => Typed.StringLit(v)(StringType)
      case NullLit() => Typed.NullLit()(NullType)

      // These are trivial, too!
      case ReadInt() => Typed.ReadInt()(IntType)
      case ReadLine() => Typed.ReadLine()(StringType)

      // Expressions. Note how we make a fair guess even when the expression doesn't type check.
      case UnaryExpr(op, operand) =>
        val e = typeExpr(operand)
        e.typ match {
          case NoType => Typed.UnaryExpr(op, e) // avoid nested errors
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
          case (_, NoType) | (NoType, _) => Typed.BinaryExpr(op, l, r) // avoid nested errors
          case (lt, rt) =>
            if (!compatible(op, lt, rt)) issue(new IncompatBinOpError(op, lt, rt, expr.pos))
            Typed.BinaryExpr(op, l, r)(resultTypeOf(op)) // same as UnaryExpr case
        }

      // Array related
      case NewArray(elemType, length) =>
        val t = typeTypeLit(elemType)
        val l = typeExpr(length)
        if (t.typ.isVoidType) issue(new BadArrElementError(elemType.pos))
        if (l.typ !== IntType) issue(new BadNewArrayLength(length.pos))
        Typed.NewArray(t, l)(ArrayType(t.typ))

      // Class related
      case NewClass(id) =>
        ctx.lookupClass(id) match {
          case Some(clazz) => Typed.NewClass(clazz)(clazz.typ)
          case None => issue(new ClassNotFoundError(id.name, expr.pos)); Typed.Ill(expr)
        }

      case This() =>
        if (ctx.currentFun.isStatic) issue(new ThisInStaticFuncError(expr.pos))
        Typed.This()(ctx.currentClass.typ)

      case call @ Call(Some(VarSel(None, id)), method, _) if ctx.containsClass(id) =>
        // special case like MyClass.foo();
        ctx.getClass(id).lookup(method) match {
          case Some(sym) => sym match {
            case m: MethodSymbol =>
              if (!m.isStatic) issue(new NotClassFieldError(method, id, expr.pos))
              typeCall(call, None, m)
            case _ => issue(new NotClassMethodError(method, id, expr.pos)); Typed.Ill(expr)
          }
          case None => issue(new FieldNotFoundError(method, id, expr.pos)); Typed.Ill(expr)
        }

      case call @ Call(receiver, method, args) =>
        val r = receiver.map(typeExpr)
        val rt = r.map(_.typ).getOrElse(ctx.currentClass.typ)
        rt match {
          case NoType => Typed.Ill(expr)
          case _: ArrayType if method.name == "length" => // special case for array.length()
            assert(r.isDefined)
            if (args.nonEmpty) issue(new BadLengthArgError(args.length, expr.pos))
            Typed.ArrayLen(r.get)(IntType)
          case ClassType(clazz, _) =>
            ctx.getClass(clazz).lookup(method) match {
              case Some(sym) => sym match {
                case m: MethodSymbol => typeCall(call, r, m)
                case _ => issue(new NotClassMethodError(method, clazz, expr.pos)); Typed.Ill(expr)
              }
              case None => issue(new FieldNotFoundError(method, clazz, expr.pos)); Typed.Ill(expr)
            }
          case t => issue(new NotClassFieldError(method, t.toString, expr.pos)); Typed.Ill(expr)
        }

      case ClassTest(obj, clazz) =>
        val o = typeExpr(obj)
        if (!o.typ.isClassType) issue(new NotClassError(o.typ, expr.pos))
        ctx.lookupClass(clazz) match {
          case Some(c) => Typed.ClassTest(o, c)(BoolType)
          case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); Typed.Ill(expr)
        }

      case ClassCast(obj, clazz) =>
        val o = typeExpr(obj)
        if (!o.typ.isClassType) issue(new NotClassError(o.typ, expr.pos))
        ctx.lookupClass(clazz) match {
          case Some(c) => Typed.ClassCast(o, c)(c.typ)
          case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); Typed.Ill(expr)
        }
    }
    typed.setPos(expr.pos)
  }

  def typeCall(call: Call, receiver: Option[Typed.Expr], method: MethodSymbol)
              (implicit ctx: ScopeContext): Typed.Expr = {
    // Cannot call this's member methods in a static method
    if (receiver.isEmpty && ctx.currentFun.isStatic && !method.isStatic)
      issue(new RefNonStaticError(method, ctx.currentFun, call.pos))

    val args = call.args
    if (method.arity != args.length)
      issue(new BadArgCountError(method.name, method.arity, args.length, call.pos))

    val as = (method.typ.params zip args).zipWithIndex.map {
      case ((t, a), i) =>
        val e = typeExpr(a)
        if (e.typ.noError && !(e.typ <= t))
          issue(new BadArgTypeError(i, t.toString, e.typ.toString, a.pos))
        e
    }

    if (method.isStatic) Typed.StaticCall(method, as)(method.returnType)
    else Typed.MemberCall(receiver.getOrElse(Typed.This()), method, as)(method.returnType)
  }

  def typeLValue(expr: LValue)(implicit ctx: ScopeContext): Typed.LValue = {
    val typed = expr match {
      // Variable, which should be complicated since a legal variable could refer to a local var,
      // a visible member var, and a class name.
      case VarSel(None, id) =>
        ctx.lookup(id) match {
          case Some(sym) => sym match {
            case v: LocalVarSymbol => Typed.LocalVar(v)(v.typ)
            case v: MemberVarSymbol =>
              if (ctx.currentFun.isStatic) // member vars cannot be accessed in a static method
                issue(new RefNonStaticError(name, ctx.currentFun.name, expr.pos))
              Typed.MemberVar(Typed.This(), v)(v.typ)
            // TODO report error: method and types are not allowed here
            case _ => issue(new UndeclVarError(name, expr.pos)); Typed.Ill(expr)
          }
          case None => issue(new UndeclVarError(id, expr.pos)); Typed.Ill(expr)
        }

      case VarSel(Some(receiver), id) =>
        val r = typeExpr(receiver)
        r.typ match {
          case NoType => Typed.Ill(expr)
          case t @ ClassType(clazz, _) =>
            ctx.getClass(clazz).lookup(id) match {
              case Some(sym) => sym match {
                case v: MemberVarSymbol =>
                  if (!(ctx.currentClass.typ <= t)) // member vars are protected
                    issue(new FieldNotAccessError(id, clazz, expr.pos))
                  Typed.MemberVar(r, v)(v.typ)
                // case m: MethodSymbol => TODO report error: method and types are not allowed here
                case _ => issue(new FieldNotFoundError(id, clazz, expr.pos)); Typed.Ill(expr)
              }
              case None => issue(new FieldNotFoundError(id, clazz, expr.pos)); Typed.Ill(expr)
            }
          case t => issue(new NotClassFieldError(id, t.toString, expr.pos)); Typed.Ill(expr)
        }

      case IndexSel(array, index) =>
        val a = typeExpr(array)
        val i = typeExpr(index)
        val typ = a.typ match {
          case ArrayType(elemType) =>
            if (i.typ !== IntType) issue(new SubNotIntError(expr.pos))
            elemType
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

  override def post(tree: Typed.Tree)(implicit config: Config): Unit = {
    implicit val printer = new IndentPrinter
    PrettyScope.pretty(tree.scope)
    if (config.target == Config.Target.PA2) {
      config.outputStream.print(printer.toString)
    }
  }
}