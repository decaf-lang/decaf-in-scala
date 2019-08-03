//package decaf.frontend.typecheck
//
//import decaf.Driver
//import decaf.frontend.core.Errors._
//import decaf.frontend.annot.Scopes._
//import decaf.frontend.symbols._
//import decaf.frontend.core.Trees._
//import decaf.frontend.annot.types._
//
//import scala.collection.mutable
//
//class Resolver extends Visitor[Unit, ScopeContext] {
//  val err = Driver.err _
//
//  override def visitTopLevel(topLevel: TopLevel, ctx: ScopeContext): Unit = {
//    val mapping = resolveDefConflict(topLevel.classes)
//    val classes = mapping.values
//
//    for (clazz <- classes) {
//      clazz.parent match {
//        case Some(Id(p)) if !mapping.keySet.contains(p) => clazz.detachParent
//        case _ =>
//      }
//    }
//
//    resolveInheritanceCycle(mapping)
//
//    def resolveType(clazz: ClassDef): ClassType = clazz.parent match {
//      case Some(Id(p)) =>
//        val t = resolveType(mapping(p))
//        ClassType(clazz.name, Some(t))
//      case None => ClassType(clazz.name)
//    }
//
//    val opened = new GlobalScope :: ctx
//    for (clazz <- classes) {
//      clazz.symbol = new ClassSymbol(clazz, resolveType(clazz))
//      opened.declare(clazz.symbol)
//    }
//
//    for (clazz <- classes) {
//      clazz.accept(this, opened)
//    }
//
//    // TODO: check override
//
//    // TODO: decide main class
//  }
//
//  override def visitClassDef(classDef: ClassDef, ctx: ScopeContext): Unit = {
//    val fields = resolveDefConflict(classDef.fields).values
//
//    val opened = new ClassScope(classDef.symbol) :: ctx
//    for (field <- fields) {
//      field.accept(this, opened)
//    }
//  }
//
//  override def visitVarDef(varDef: VarDef, ctx: ScopeContext): Unit = {
//    if (varDef.typ.isVoid) { // member variable cannot be of type void
//      err(new BadVarTypeError(varDef.id.name, varDef.pos))
//      return
//    }
//
//    varDef.typ.accept(this, ctx)
//    varDef.symbol = new VarSymbol(varDef, varDef.typ.typ)
//    ctx.declare(varDef.symbol)
//  }
//
//  override def visitMethodDef(methodDef: MethodDef, ctx: ScopeContext): Unit = {
//    methodDef.sig.params.foreach(_.accept(this, ctx))
//    methodDef.sig.returnType.accept(this, ctx)
//    methodDef.symbol = new FunSymbol(methodDef, ???)
//    ctx.declare(methodDef.symbol)
//
//    val opened = new FormalScope(methodDef.body, methodDef.symbol) :: ctx
//    methodDef.body.accept(this, opened)
//  }
//
//  override def visitTInt(typ: TInt, ctx: ScopeContext): Unit = typ.typ = IntType
//
//  override def visitTBool(typ: TBool, ctx: ScopeContext): Unit = typ.typ = BoolType
//
//  override def visitTString(typ: TString, ctx: ScopeContext): Unit = typ.typ = StringType
//
//  override def visitTVoid(typ: TVoid, ctx: ScopeContext): Unit = typ.typ = VoidType
//
//  override def visitTArray(typ: TArray, ctx: ScopeContext): Unit = {
//    typ.elemType match {
//      case TVoid() => err(new BadArrElementError(typ.elemType.pos))
//      case _ =>
//        typ.elemType.accept(this)
//        typ.typ = ArrayType(typ.elemType.typ)
//    }
//  }
//
//  override def visitTClass(typ: TClass, ctx: ScopeContext): Unit = {
//    ctx.lookupClass(typ.id.name) match {
//      case Some(sym) =>
//        typ.typ = sym.typ
//      case None => err(new ClassNotFoundError(typ.id.name, typ.id.pos))
//    }
//  }
//
//  override def visitIf(stmt: If, ctx: ScopeContext): Unit = {
//    stmt.trueBranch.accept(this, ctx)
//    stmt.falseBranch.foreach(_.accept(this, ctx))
//  }
//
//  override def visitWhile(stmt: While, ctx: ScopeContext): Unit = {
//    stmt.body.accept(this, ctx)
//  }
//
//  override def visitFor(stmt: For, ctx: ScopeContext): Unit = {
//    stmt.body.accept(this, ctx)
//  }
//
//  override def visitBlock(stmt: Block, ctx: ScopeContext): Unit = {
//    val opened = new LocalScope(stmt) :: ctx
//    for (stmt <- stmt.stmts) {
//      stmt.accept(this, opened)
//    }
//  }
//
//  private def resolveDefConflict[T <: Def](defs: List[T]): mutable.HashMap[String, T] = {
//    val map = new mutable.HashMap[String, T]
//    for (d <- defs) {
//      map.get(d.name) match {
//        case Some(earlier) =>
//          err(new DeclConflictError(d.name, earlier.pos, d.pos))
//        case None =>
//          map(d.name) = d
//      }
//    }
//    map
//  }
//
//  private def resolveInheritanceCycle(mapping: mutable.HashMap[String, ClassDef]): Unit = {
//    val visited = new mutable.HashSet[String]
//
//    def visit(from: ClassDef, name: String): Unit = {
//      if (visited(name)) {
//        err(new BadInheritanceError(from.pos))
//        from.detachParent
//      } else {
//        visited.add(name)
//      }
//
//      mapping(name).parent match {
//        case Some(Id(p)) => visit(from, name)
//        case None =>
//      }
//    }
//
//    for ((name, clazz) <- mapping) {
//      if (!visited(name)) visit(clazz, name)
//    }
//  }
//}
