import decaf.error._
import decaf.frontend.annot.ScopedImplicit._
import decaf.frontend.annot.SymbolizedImplicit._
import decaf.frontend.annot.TypedImplicit._
import decaf.frontend.annot._
import decaf.frontend.tree.ResolvedTree._
import decaf.frontend.tree.SyntaxTree.{ClassDef => _, MethodDef => _, Tree => _, VarDef => _, _}
import decaf.frontend.tree.TreeNode._
import decaf.frontend.tree.TypedTree.TypeVar
import decaf.frontend.tree.{SyntaxTree, TypedTree => Typed}
import decaf.frontend.typecheck.TypeLitResolver
import decaf.schedule.Phase

class Typer extends Phase("typer") with TypeLitResolver {
  type Input = Tree
  type Output = Typed.Tree

  override def transform(tree: Tree): Typed.Tree = {
    val global = new ScopeContext(tree.scope)
    val checkedClasses = tree.classes.map {
      case clazz@ClassDef(id, parent, fields) =>
        val symbol = clazz.symbol
        val ctx = global.open(symbol.scope)
        val checkedFields = fields.map {
          case f: VarDef => transformVarDef(f)
          case f@MethodDef(isStatic, returnType, id, params, body) =>
            val symbol = f.symbol
            val checkedParams = params.map(transformVarDef)
            val checkedBody = checkBlock(ctx.open(symbol.scope))(body)
            // TODO check return
            Typed.MethodDef(isStatic, returnType, id, checkedParams, checkedBody)(symbol).setPos(f.pos)
        }
        Typed.ClassDef(id, parent, checkedFields)(symbol).setPos(clazz.pos)
    }

    Typed.TopLevel(checkedClasses)(tree.scope).setPos(tree.pos)
  }

  def transformVarDef(varDef: VarDef): Typed.VarDef = varDef match {
    case VarDef(typeLit, id) => Typed.VarDef(typeLit, id)(varDef.symbol).setPos(varDef.pos)
  }

  def checkBlock(ctx: ScopeContext, insideLoop: Boolean = false)(block: SyntaxTree.Block): Typed.Block = {
    val local = ctx.open(new LocalScope)
    val ss = block.stmts.flatMap(check(local, insideLoop))
    Typed.Block(ss).setPos(block.pos)
  }

  object ASS extends BinaryOp

  def checkSimpleStmt(ctx: ScopeContext, insideLoop: Boolean)(stmt: SimpleStmt): Option[Typed.SimpleStmt] = {
    val checked = stmt match {
      case Assign(lhs, rhs) =>
        val l = typing(ctx)(lhs)
        val r = typing(ctx)(rhs)
        l.typ match {
          case NoType => None
          case _: FunType => issue(new IncompatBinOpError(ASS, l.typ, r.typ, stmt.pos)); None
          case t if !(r.typ sub t) => issue(new IncompatBinOpError(ASS, l.typ, r.typ, stmt.pos)); None
          case _ => Some(Typed.Assign(l, r))
        }

      case ExprEval(expr) =>
        val e = typing(ctx)(expr)
        e.typ match {
          case NoType => None
          case _ => Some(Typed.ExprEval(e))
        }

      case Skip() => Some(Typed.Skip())
    }
    checked.map(_.setPos(stmt.pos))
  }

  def check(ctx: ScopeContext, insideLoop: Boolean)(stmt: Stmt): Option[Typed.Stmt] = stmt match {
    case block: Block => Some(checkBlock(ctx, insideLoop)(block))
    case simple: SimpleStmt => checkSimpleStmt(ctx, insideLoop)(simple)
    case _ =>
      val checked = stmt match {
        case varDef@LocalVarDef(typeLit, id) =>
          ctx.search(id.name) match {
            case Some(earlier) =>
              issue(new DeclConflictError(id.name, earlier.pos, varDef.pos))
              None
            case None =>
              val typedTypeLit = resolveTypeLit(typeLit, ctx)
              typedTypeLit.typ match {
                case NoType => None
                case VoidType => issue(new BadVarTypeError(id.name, varDef.pos)); None
                case t =>
                  val symbol = new VarSymbol(varDef, t)
                  ctx.declare(symbol)
                  Some(Typed.LocalVarDef(typedTypeLit, id)(symbol).setPos(varDef.pos))
              }
          }

        case If(cond, trueBranch, falseBranch) =>
          val c = checkTestExpr(ctx)(cond)
          for {
            t <- check(ctx, insideLoop)(trueBranch)
            f <- falseBranch.map(check(ctx, insideLoop))
          } yield Typed.If(c, t, f)

        case While(cond, body) =>
          val c = checkTestExpr(ctx)(cond)
          for {
            b <- check(ctx, true)(body)
          } yield Typed.While(c, b)

        case For(init, cond, update, body) =>
          for {
            i <- checkSimpleStmt(ctx, insideLoop)(init)
            c = checkTestExpr(ctx)(cond)
            u <- checkSimpleStmt(ctx, insideLoop)(update)
            b <- check(ctx, true)(body)
          } yield Typed.For(i, c, u, b)

        case Break() =>
          if (insideLoop) Some(Typed.Break())
          else issue(new BreakOutOfLoopError(stmt.pos));
          None

        case Return(expr) =>
          val expected = ctx.currentFun.returnType
          val typed = expr.map(typing(ctx))
          val actual = typed match {
            case Some(e) => e.typ
            case None => VoidType
          }
          actual match {
            case NoType => None
            case _ if actual sub expected => Some(Typed.Return(typed))
            case _ => issue(new BadReturnTypeError(expected, actual, stmt.pos)); None
          }

        case Print(exprs) =>
          val es = exprs.zipWithIndex.flatMap {
            case (expr, i) =>
              val e = typing(ctx)(expr)
              e.typ match {
                case NoType | _: BaseType => Some(e)
                case t => issue(new BadPrintArgError(i, t, expr.pos)); None
              }
          }
          Some(Typed.Print(es))
      }
      checked.map(_.setPos(stmt.pos))
  }

  def checkTestExpr(ctx: ScopeContext)(expr: SyntaxTree.Expr): Typed.Expr = {
    val e = typing(ctx)(expr)
    if (e.typ ne BoolType) issue(new BadTestExpr(expr.pos))
    e
  }

  def typing(ctx: ScopeContext)(expr: SyntaxTree.Expr): Typed.Expr = {
    implicit val noType: NoType.type = NoType

    val typed = expr match {
      // Trivial cases for literals
      case IntLit(value) => Typed.IntLit(value)(IntType)
      case BoolLit(value) => Typed.BoolLit(value)(BoolType)
      case StringLit(value) => Typed.StringLit(value)(StringType)
      case NullLit() => Typed.NullLit()(NullType)

      // These are trivial, too!
      case ReadInt() => Typed.ReadInt()(IntType)
      case ReadLine() => Typed.ReadLine()(StringType)

      // Expressions. Note how we make a fair guess even when the expression doesn't type check.
      case UnaryExpr(op, operand) =>
        val e = typing(ctx)(operand)
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
        val l = typing(ctx)(lhs)
        val r = typing(ctx)(rhs)
        (l.typ, r.typ) match {
          case (_, NoType) | (NoType, _) => Typed.BinaryExpr(op, l, r) // avoid nested errors
          case (lt, rt) =>
            if (!compatible(op, lt, rt)) issue(new IncompatBinOpError(op, lt, rt, expr.pos))
            Typed.BinaryExpr(op, l, r)(resultTypeOf(op)) // same as UnaryExpr case
        }

      // Variable, which should be complicated since a legal variable could refer to a local var,
      // a visible member var, and a class name.
      case Var(id) =>
        ctx.lookup(id.name) match {
          case Some(symbol) =>
            symbol match {
              case v: VarSymbol =>
                if (ctx.currentFun.isStatic && v.kind == MemberVar) // member vars cannot be accessed in a static method
                  issue(new RefNonStaticError(id.name, ctx.currentFun.name, expr.pos))
              case _ =>
            }
            Typed.Var(id)(symbol.typ)

          case None => issue(new UndeclVarError(id.name, expr.pos)); Typed.Var(id)
        }

      // Array related
      case NewArray(elemType, length) =>
        val t = resolveTypeLit(elemType, ctx)
        val l = typing(ctx)(length)
        val typ = t.typ match {
          case NoType => NoType // avoid nested errors
          case VoidType => issue(new BadArrElementError(elemType.pos)); NoType
          case t =>
            if (l.typ ne IntType) issue(new BadNewArrayLength(length.pos))
            ArrayType(t)
        }
        Typed.NewArray(t, l)(typ)

      case IndexSel(array, index) =>
        val a = typing(ctx)(array)
        val i = typing(ctx)(index)
        val typ = a.typ match {
          case ArrayType(elemType) => if (i.typ ne IntType) issue(new SubNotIntError(expr.pos)); elemType
          case _ => issue(new NotArrayError(array.pos)); NoType
        }
        Typed.IndexSel(a, i)(typ)

      // Class related
      case NewClass(id) =>
        val typ = ctx.lookupClass(id.name) match {
          case Some(symbol) => symbol.typ
          case None => issue(new ClassNotFoundError(id.name, expr.pos)); NoType
        }
        Typed.NewClass(id)(typ)

      case This() =>
        val typ = if (ctx.currentFun.isStatic) {
          issue(new ThisInStaticFuncError(expr.pos))
          NoType
        } else ctx.currentClass.typ
        Typed.This()(typ)

      case FieldSel(receiver, field) =>
        val r = typing(ctx)(receiver)
        val typ = r.typ match {
          case NoType => NoType // avoid nested errors
          case t@ClassType(c, _) if !r.isInstanceOf[TypeVar] => // make sure it's not a type var
            ctx.getClass(c).lookup(field.name) match {
              case Some(sym) => sym match {
                case v: VarSymbol => // be careful that v may not be accessible, since vars are protected!
                  if (ctx.currentClass.typ sub t) v.typ
                  else {
                    issue(new FieldNotAccessError(field.name, c, expr.pos))
                    NoType
                  }
                case _ => sym.typ
              }
              case None => issue(new FieldNotFoundError(field.name, c, expr.pos)); NoType
            }
          case t => issue(new NotClassFieldError(field.name, t.toString, expr.pos)); NoType
        }
        Typed.FieldSel(r, field)(typ)

      case Call(receiver, method, args) =>
        def checkArgs(test: FunSymbol => Boolean, error: Error,
                      receiverType: Type): (Type, List[Typed.Expr]) = {
          receiverType match {
            case NoType => (NoType, Nil)
            case _: ArrayType if method.name == "length" => // special case for array.length()
              if (args.isEmpty) (IntType, Nil)
              else {
                issue(new BadLengthArgError(args.length, expr.pos))
                (NoType, Nil)
              }
            case ClassType(c, _) => // casual case for calling a method of class c
              ctx.getClass(c).lookup(method.name) match {
                case Some(sym) => sym match {
                  case f: FunSymbol =>
                    if (!test(f)) issue(error)
                    if (f.arity != args.length)
                      issue(new BadArgCountError(method.name, f.arity, args.length, expr.pos))
                    val as = (f.typ.params zip args).zipWithIndex.map {
                      case ((t, a), i) =>
                        val e = typing(ctx)(a)
                        if (e.typ.noError && !(e.typ sub t))
                          issue(new BadArgTypeError(i, t.toString, e.typ.toString, a.pos))
                        e
                    }
                    (f.returnType, as)
                  case _ => issue(new NotClassMethodError(method.name, c, expr.pos)); (NoType, Nil)
                }
                case None => issue(new FieldNotFoundError(method.name, c, expr.pos)); (NoType, Nil)
              }
            case t => issue(new NotClassFieldError(method.name, t.toString, expr.pos)); (NoType, Nil)
          }
        }

        val typed = receiver.map(typing(ctx))
        val (typ, as) = typed match {
          case Some(Typed.TypeVar(_, t)) =>
            checkArgs(_.isStatic, new NotClassFieldError(method.name, t.toString, expr.pos), t)
          case Some(e) => checkArgs(_ => true, null, e.typ)
          case None => checkArgs(x => if (ctx.currentFun.isStatic) x.isStatic else true,
            new RefNonStaticError(method.name, ctx.currentFun.name, expr.pos), ctx.currentClass.typ)
        }
        Typed.Call(typed, method, as)(typ)

      case ClassTest(obj, clazz) =>
        val o = typing(ctx)(obj)
        if (!o.typ.isClassType) issue(new NotClassError(o.typ, expr.pos))
        if (ctx.lookupClass(clazz.name).isEmpty) issue(new ClassNotFoundError(clazz.name, expr.pos))
        Typed.ClassTest(o, clazz)(BoolType)

      case ClassCast(obj, clazz) =>
        val o = typing(ctx)(obj)
        if (!o.typ.isClassType) issue(new NotClassError(o.typ, expr.pos))
        val typ = ctx.lookupClass(clazz.name) match {
          case Some(symbol) => symbol.typ
          case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); NoType
        }
        Typed.ClassCast(o, clazz)(typ)
    }
    typed.setPos(expr.pos)
  }

  def compatible(op: UnaryOp, operand: Type): Boolean = op match {
    case NEG => operand eq IntType // if e : int, then -e : int
    case NOT => operand eq BoolType // if e : bool, then !e : bool
  }

  def compatible(op: BinaryOp, lhs: Type, rhs: Type): Boolean = op match {
    case _: ArithOp => (lhs eq IntType) && (rhs eq IntType) // if e1, e2 : int, then e1 + e2 : int
    case _: LogicOp => (lhs eq BoolType) && (rhs eq BoolType) // if e1, e2 : bool, then e1 && e2 : bool
    case _: EqOp => (lhs sub rhs) || (rhs sub lhs) // if e1 : T1, e2 : T2, T1 <: T2 or T2 <: T1, then e1 == e2 : bool
    case _: CmpOp => (lhs eq IntType) && (rhs eq IntType) // if e1, e2 : int, then e1 > e2 : bool
  }

  def resultTypeOf(op: UnaryOp): Type = op match {
    case NEG => IntType
    case NOT => BoolType
  }

  def resultTypeOf(op: BinaryOp): Type = op match {
    case _: ArithOp => IntType
    case _: LogicOp | _: EqOp | _: CmpOp => BoolType
  }
}