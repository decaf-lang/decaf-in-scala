import decaf.error._
import decaf.frontend.annot.TypedImplicit._
import decaf.frontend.annot._
import decaf.frontend.tree.TreeNode._
import decaf.frontend.tree.{ResolvedTree, SyntaxTree, TypedTree}
import decaf.frontend.typecheck.TypeLitResolver
import decaf.schedule.Phase

class Typer extends Phase("typer") with TypeLitResolver {
  type Input = ResolvedTree.Tree
  type Output = TypedTree.Tree

  override def transform(input: ResolvedTree.Tree): TypedTree.Tree = ???

  def check(stmt: SyntaxTree.Stmt, ctx: ScopeContext): TypedTree.Stmt = ???

  def typing(expr: SyntaxTree.Expr, ctx: ScopeContext): TypedTree.Expr = expr match {
    // Trivial cases for literals
    case IntLit(value, _) => IntLit(value, IntType)
    case BoolLit(value, _) => BoolLit(value, BoolType)
    case StringLit(value, _) => StringLit(value, StringType)
    case NullLit(_) => NullLit(NullType)

    // These are trivial, too!
    case ReadInt(_) => ReadInt(IntType)
    case ReadLine(_) => ReadInt(StringType)

    // Expressions. Note how we make a fair guess even when the expression doesn't type check.
    case UnaryExpr(op, operand, _) =>
      val typedOperand = typing(operand, ctx)
      typedOperand.typ match {
        case NoType => UnaryExpr(op, typedOperand, NoType) // avoid nested errors
        case t =>
          if (!compatible(op, t)) issue(new IncompatUnOpError(op, t, expr.pos))
          // Even when it doesn't type check, we could make a fair guess based on the operator kind.
          // Let's say the operator is `-`, then one possibly wants an integer as the operand.
          // Once he/she fixes the operand, according to our type inference rule, the whole unary expression
          // must have type int! Thus, we simply _assume_ it has type int, rather than `NoType`.
          UnaryExpr(op, typedOperand, resultTypeOf(op))
      }

    case BinaryExpr(op, lhs, rhs, _) =>
      val typedLhs = typing(lhs, ctx)
      val typedRhs = typing(rhs, ctx)
      (typedLhs.typ, typedRhs.typ) match {
        case (_, NoType) | (NoType, _) => BinaryExpr(op, typedLhs, typedRhs, NoType) // avoid nested errors
        case (l, r) =>
          if (!compatible(op, l, r)) issue(new IncompatBinOpError(op, l, r, expr.pos))
          BinaryExpr(op, typedLhs, typedRhs, resultTypeOf(op)) // same as UnaryExpr case
      }

    // Array related
    case NewArray(elemType, length, _) =>
      val typedElemType = resolveTypeLit(elemType, ctx)
      val typedLength = typing(length, ctx)
      val typ = typedElemType.typ match {
        case NoType => NoType // avoid nested errors
        case VoidType => issue(new BadArrElementError(elemType.pos)); NoType
        case t =>
          if (typedLength.typ ne IntType) issue(new BadNewArrayLength(length.pos))
          ArrayType(t)
      }
      NewArray(typedElemType, typedLength, typ)

    case IndexSel(array, index, _) =>
      val typedArray = typing(array, ctx)
      val typedIndex = typing(index, ctx)
      val typ = typedArray.typ match {
        case ArrayType(elemType) =>
          if (typedIndex.typ ne IntType) issue(new SubNotIntError(expr.pos))
          elemType
        case _ => issue(new NotArrayError(array.pos)); NoType
      }
      IndexSel(typedArray, typedIndex, typ)

    // Class related
    case NewClass(id, _) =>
      val typ = ctx.lookupClass(id.name) match {
        case Some(symbol) => symbol.typ
        case None => issue(new ClassNotFoundError(id.name, expr.pos)); NoType
      }
      NewClass(id, typ)

    case This(_) =>
      val typ = if (ctx.currentFun.isStatic) {
        issue(new ThisInStaticFuncError(expr.pos))
        NoType
      } else ctx.currentClass.typ
      This(typ)

    case FieldSel(receiver, field, _) =>
      val typedReceiver = typing(receiver, ctx)
      val typ = typedReceiver.typ match {
        case NoType => NoType // avoid nested errors
        // TODO receiver should not be a class ref
        //        case c: ClassType => c.lookup(field.name) match {
        //          case None | Some(FunType(_, _)) => issue(new FieldNotFoundError(field.name, c.name, expr.pos)); NoType
        //          case Some(t) => t
        //        }
        case t => issue(new NotClassFieldError(field.name, t.toString, expr.pos)); NoType
      }
      FieldSel(typedReceiver, field, typ)

    case Call(receiver, method, args, annot) =>
      val typedReceiver = receiver.map(typing(_, ctx))
      val receiverType = typedReceiver match {
        case Some(r) => r.typ
        case None => ctx.currentClass.typ
      }
      val typ = receiverType match {
        case NoType => NoType
        // TODO
        //        case c: ClassType => c.lookup(method.name) match {
        //          case Some(FunType(_, _)) => ???
        //          case Some(_) => issue(new NotClassMethodError(method.name, c.name, expr.pos)); NoType
        //          case None => issue(new FieldNotFoundError(method.name, c.name, expr.pos)); NoType
        //        }
        case t => issue(new NotClassFieldError(method.name, t.toString, expr.pos)); NoType
      }
      Call(typedReceiver, method, ???, typ)

    case ClassTest(obj, clazz, _) =>
      val typedObj = typing(obj, ctx)
      if (!typedObj.typ.isInstanceOf[ClassType]) issue(new NotClassError(typedObj.typ, expr.pos))
      if (ctx.lookupClass(clazz.name).isEmpty) issue(new ClassNotFoundError(clazz.name, expr.pos))
      ClassTest(typedObj, clazz, BoolType)

    case ClassCast(obj, clazz, _) =>
      val typedObj = typing(obj, ctx)
      if (!typedObj.typ.isInstanceOf[ClassType]) issue(new NotClassError(typedObj.typ, expr.pos))
      val typ = ctx.lookupClass(clazz.name) match {
        case Some(symbol) => symbol.typ
        case None => issue(new ClassNotFoundError(clazz.name, expr.pos)); NoType
      }
      ClassCast(typedObj, clazz, typ)
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