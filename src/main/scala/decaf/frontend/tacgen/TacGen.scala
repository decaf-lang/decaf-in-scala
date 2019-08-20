package decaf.frontend.tacgen

import java.io.PrintWriter

import decaf.driver.{Config, Phase}
import decaf.frontend.annot.SymbolizedImplicit._
import decaf.frontend.annot.TypedImplicit._
import decaf.frontend.annot._
import decaf.frontend.tac.Tac._
import decaf.frontend.tac._
import decaf.frontend.tree.TreeNode
import decaf.frontend.tree.TypedTree._

import scala.collection.mutable

class TacGen extends Phase[Tree, Program]("tacgen") with Util {

  type ClassVTables = mutable.HashMap[ClassSymbol, VTable]
  type MemberOffsets = mutable.HashMap[Symbol, Int]
  type MethodLabels = mutable.HashMap[Symbol, Label]
  type VarTemps = mutable.HashMap[LocalVarSymbol, Temp]

  class GlobalContext {
    val vtbl: ClassVTables = new ClassVTables
    val offset: MemberOffsets = new MemberOffsets
    val label: MethodLabels = new MethodLabels
  }

  override def transform(tree: Tree): Program = {
    val classes = tree.classes
    implicit val ctx = new GlobalContext

    // Generate labels for every method
    classes.foreach { clazz =>
      ctx.label(clazz.symbol) = Label.fresh(s"_${ clazz.name }_New") // the "New" method for initialization
      clazz.symbol.methods.foreach { method => // the real methods defined in the program
        ctx.label(method) = Label.fresh(if (method.isMain) "main" else s"_${ clazz.name }.${ method.name }")
      }
    }

    // Generate vtables and assign offsets to every member method
    classes.foreach { clazz => genVTableAndOffsets(clazz.symbol) }

    // Generate the default static "New" method for every class
    val procs1 = classes.map { clazz => genNewProc(clazz.symbol) }

    // Generate all static and member methods defined in the program
    val procs2 = for {
      clazz <- classes
      method <- clazz.methods
    } yield genProc(method)

    Program(ctx.vtbl.values.toList, procs1 ++ procs2)
  }

  def genVTableAndOffsets(clazz: ClassSymbol)(implicit ctx: GlobalContext): Unit = {
    if (!ctx.vtbl.contains(clazz)) {
      val parent = clazz.parent.map { base => genVTableAndOffsets(base); ctx.vtbl(base) }
      val inheritedMethods = parent.map(_.memberMethods).getOrElse(Nil)
      val inheritedVars = parent.map(_.memberVars).getOrElse(Nil)

      // Before we scan the actual entries defined in this class, first copy every inherited entry.
      val methods = new mutable.ArrayBuffer[Label].addAll(inheritedMethods)
      val vars = new mutable.ArrayBuffer[MemberVarSymbol].addAll(inheritedVars)

      // For every newly defined method in this class:
      // - If it overrides an inherited method, replace that with this one and retain the offset.
      // - Otherwise, append it to the end of the list, and increase the offset.
      clazz.memberMethods.foreach { m =>
        m.overrides match {
          case Some(base) =>
            ctx.offset(m) = ctx.offset(base)
            val index = ctx.offset(base) / 4 - 2
            methods(index) = ctx.label(m)
          case None =>
            ctx.offset(m) = 8 + 4 * methods.length
            methods.append(ctx.label(m))
        }
      }

      // For every newly defined var in this class:
      // - Simply append it to the end of the list, and increase the offset.
      clazz.vars.foreach { v =>
        ctx.offset(v) = 4 + 4 * vars.length
        vars.append(v)
      }

      ctx.vtbl(clazz) = new VTable(s"_${ clazz.name }", clazz.name, parent, methods.toList, vars.toList)
    }
  }

  def genNewProc(clazz: ClassSymbol)(implicit ctx: GlobalContext): Proc = {
    val label = ctx.label(clazz)
    val count = ctx.vtbl(clazz).memberVars.length
    val obj = Mark(label) || load(4 + count * 4) >> { intrinsicCall(Lib.ALLOCATE, _) }
    val zero = load(0)
    val init = List.tabulate(count) { i => Store(zero, obj, 4 * (i + 1)) }
    val vtLoader = emit(LoadVTbl)(ctx.vtbl(clazz)) >| { v => Store(v, obj, 0) }
    val code = obj || zero || init || vtLoader || Ret(obj)
    new Proc(label, Memo(), code.seq)
  }

  def genProc(method: MethodDef)(implicit ctx: GlobalContext): Proc = {
    val label = ctx.label(method.symbol)
    val self = if (method.isStatic) null else Temp.fresh
    implicit val localCtx: Context = new Context(ctx, self)

    val base = (if (method.isStatic) 0 else 4) + 4
    val ms = method.params.zipWithIndex.map {
      case (v, i) =>
        val t = Temp.fresh
        localCtx.temp(v.symbol) = t
        val offset = base + 4 * i
        s"$t:$offset"
    }
    val memos = if (method.isStatic) ms else s"$self:4" :: ms
    val paramMemo = Memo(memos.mkString(" "))

    implicit val loopExits: List[Label] = Nil
    val code = Mark(label) || emitStmt(method.body)
    new Proc(label, paramMemo, code.seq)
  }

  class Context(global: GlobalContext, val self: Temp) {
    val temp: VarTemps = new VarTemps
    val vtbl: ClassVTables = global.vtbl
    val offset: MemberOffsets = global.offset
    val label: MethodLabels = global.label
  }

  def emitStmt(stmt: Stmt)(implicit loopExits: List[Label], ctx: Context): InstrBlock = stmt match {
    case Block(stmts) => stmts.map(emitStmt).flatMap(_.seq)

    case v: LocalVarDef =>
      ctx.temp(v.symbol) = Temp.fresh
      Nil

    case Assign(lhs, rhs) =>
      val e = emitExpr(rhs)
      lhs match {
        case IndexSel(array, index) =>
          val a = emitExpr(array)
          val i = emitExpr(index)
          val ref = arrayElemRef(a, i)
          a || i || ref || e || Store(e, ref, 0)
        case MemberVar(receiver, v) =>
          val obj = emitExpr(receiver)
          obj || e || Store(e, obj, ctx.offset(v))
        case LocalVar(v) => e || Move(ctx.temp(v), e)
      }
    case ExprEval(expr) => emitExpr(expr)
    case Skip() => Nil

    case If(cond, trueBranch, falseBranch) =>
      emitExpr(cond) >| ifThenElse(emitStmt(trueBranch), emitStmt(falseBranch))
    case While(cond, body) =>
      val exit = Label.fresh()
      loop(emitExpr(cond), exit) { emitStmt(body)(exit :: loopExits, ctx) }
    case For(init, cond, update, body) =>
      val exit = Label.fresh()
      emitStmt(init) || loop(emitExpr(cond), exit) { emitStmt(body)(exit :: loopExits, ctx) || emitStmt(update) }
    case Break() => Branch(loopExits.head)
    case Return(None) => Ret(null)
    case Return(Some(expr)) => emitExpr(expr) >| { e => Ret(e) }
    case Print(exprs) =>
      exprs.map { expr =>
        val fun = expr.typ match {
          case IntType => Lib.PRINT_INT
          case BoolType => Lib.PRINT_BOOL
          case StringType => Lib.PRINT_STRING
        }
        emitExpr(expr) >> { e => intrinsicCall(fun, e) }
      } flatMap (_.seq)
  }

  def emitExpr(expr: Expr)(implicit ctx: Context): InstrBlockValued = expr match {
    // Literals: load immediate numbers/string
    case IntLit(v) => load(v)
    case BoolLit(v) => load(if (v) 1 else 0)
    case StringLit(v) => load(v)
    case NullLit() => load(0)

    // Prebuilt functions: intrinsic calls
    case ReadInt() => intrinsicCall(Lib.READ_INT)
    case ReadLine() => intrinsicCall(Lib.READ_LINE)

    // Unary expressions
    case UnaryExpr(TreeNode.NEG, expr) => emitExpr(expr) >> emit(Neg)
    case UnaryExpr(TreeNode.NOT, expr) => emitExpr(expr) >> emit(LNot)

    // Binary expressions
    case BinaryExpr(op, lhs, rhs) =>
      val l = emitExpr(lhs)
      val r = emitExpr(rhs)
      val e = op match {
        case TreeNode.EQ if lhs.typ === StringType => intrinsicCall(Lib.STRING_EQUAL, l, r)
        case TreeNode.NE if lhs.typ === StringType => intrinsicCall(Lib.STRING_EQUAL, l, r) >> emit(LNot)
        case _ => binary(op, l, r)
      }
      l || r || e

    // Local variables: they must be already assigned to a temp
    case LocalVar(v) => ctx.temp(v)

    // Array related
    case NewArray(_, len) => emitExpr(len) >> newArray
    case IndexSel(array, index) =>
      val arr = emitExpr(array)
      val idx = emitExpr(index)
      arr || idx || arrayElemRef(arr, idx) >> loadWith()
    case ArrayLen(array) => emitExpr(array) >> loadWith(-WORD_SIZE)

    // Class related
    case NewClass(clazz) => directCall(ctx.label(clazz))
    case This() => ctx.self
    case MemberVar(receiver, v) => emitExpr(receiver) >> loadWith(ctx.offset(v))
    case StaticCall(method, args) =>
      val es = args.map(emitExpr)
      es.flatMap(_.seq) || directCall(ctx.label(method), es.map(_.value))
    case MemberCall(receiver, method, args) =>
      val actuals = receiver :: args
      val es = actuals.map(emitExpr)
      es.flatMap(_.seq) || load(es.head) >> loadWith(ctx.offset(method)) >> indirectCall(es.map(_.value))

    case ClassTest(obj, clazz) =>
      if (obj.typ <= clazz.typ) load(1)
      else emitExpr(obj) >> classTest(ctx.vtbl(clazz))
    case ClassCast(obj, clazz) => emitExpr(obj) >> { o =>
      if (obj.typ <= clazz.typ) o
      else classCast(o, ctx.vtbl(clazz))
    }
  }

  override def post(program: Program)(implicit config: Config): Unit = {
    if (config.needsOutput(Config.Phase.tac)) {
      val code = program.toString
      val path = config.getOutputPath(".tac")
      new PrintWriter(path.toFile) {
        write(code)
        close()
      }
    }
  }
}
