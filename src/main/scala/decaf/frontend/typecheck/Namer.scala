package decaf.frontend.typecheck

import decaf.driver.Phase
import decaf.error._
import decaf.frontend.annot.SymbolizedImplicit._
import decaf.frontend.annot.TypedImplicit._
import decaf.frontend.annot._
import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.TreeNode._
import decaf.frontend.tree.{NamedTree => Named}

import scala.collection.mutable

class Namer extends Phase[Tree, Named.Tree]("namer") with Util {

  class Context {
    val global: GlobalScope = new GlobalScope
    val classes: mutable.HashMap[String, ClassDef] = new mutable.HashMap
  }

  override def transform(tree: Tree): Named.Tree = {
    implicit val ctx = new Context

    // Check conflicting definitions. If any, ignore the redefined ones.
    tree.classes.foreach {
      clazz =>
        ctx.classes.get(clazz.name) match {
          case Some(earlier) => issue(new DeclConflictError(clazz.name, earlier.pos, clazz.pos))
          case None => ctx.classes(clazz.name) = clazz
        }
    }

    // Make sure the base class exists. If not, ignore the inheritance.
    ctx.classes.foreach {
      case (name, clazz) =>
        clazz.parent match {
          case Some(Id(base)) if !ctx.classes.contains(base) =>
            issue(new ClassNotFoundError(base, clazz.pos))
            ctx.classes(name) = clazz.parentDetached
          case _ =>
        }
    }

    // Make sure any inheritance does not form a cycle. If so, eliminate the cycle by cutting an edge on the cycle.
    // Example: suppose A extends B, B extends C and C extends A, then we see A -> B -> C -> A forms a cycle.
    // We cut the last edge C -> A by detaching C's parent.
    eliminateCycles

    // So far, class inheritance is well-formed, i.e. inheritance relations form a forest of trees. Now we need to
    // resolve every class definition, make sure that every member (variables and methods) is well-typed. However,
    // since all defined classes are visible to every other class, which means we can access another class's members,
    // and of course the type that itself represents, e.g.
    //   class A {
    //     class B foo; // access B
    //     void bar() {
    //       foo.baz(); // access baz of B
    //     }
    //   }
    //   class B {
    //     void baz();
    //   }
    // Apparently, classes cannot be resolved in the order they presented in the tree: class A refers to class B,
    // whose definition goes later. To tackle this issue, one possible way is to first scan all classes "roughly"
    // and then step into details of every method definition -- because at that time, signatures of class members are
    // known. That's why we split the type checking phase into two passes: In the Resolver pass, we only scan class
    // members, while ignoring any method body, because that's enough for us to know what a class looks like.
    // In the Typer pass, we then step into every method body and type check every single statement and expression.
    // Realizing that a class type can be used in the definition of a class member, either a variable or a method,
    // we shall first know all the accessible class types in the program. These types are wrapped into what we called
    // `ClassSymbol`s. Note that currently, the associated `scope` is empty because member resolving has not started
    // yet. All class symbols are stored in the global scope.
    createClassSymbols

    // Now, we can resolve every class definition to fill in its class scope table. To check if the overriding
    // behaves correctly, we should first resolve a base class and then its subclasses.
    val resolvedClasses = resolveClasses

    // Finally, let's locate the main class, whose name is 'Main', and contains a method like:
    //  static void main() { ... }
    resolvedClasses.find(_.name == "Main") match {
      case Some(clazz) =>
        clazz.symbol.scope.lookup("main") match {
          case Some(symbol) =>
            symbol match {
              case f: MethodSymbol if f.isStatic && (f.typ === FunType(Nil, VoidType)) => f.setMain()
              case _ => issue(NoMainClassError)
            }
          case _ => issue(NoMainClassError)
        }
      case None => issue(NoMainClassError)
    }

    Named.TopLevel(resolvedClasses)(ctx.global).setPos(tree.pos)
  }

  private def eliminateCycles(implicit ctx: Context): Unit = {
    val visitedTime = new mutable.HashMap[String, Int]
    ctx.classes.keys.foreach { visitedTime(_) = 0 }

    @scala.annotation.tailrec
    def visit(from: ClassDef, node: String, time: Int): Unit = {
      if (visitedTime(node) == 0) { // not visited yet
        visitedTime(node) = time
        val clazz = ctx.classes(node)
        clazz.parent match {
          case Some(Id(base)) => visit(clazz, base, time)
          case _ => // done
        }
      } else if (visitedTime(node) == time) { // find a cycle
        issue(new BadInheritanceError(from.pos))
        ctx.classes(from.name) = from.parentDetached
      } // else: this node is visited earlier, also done
    }

    var time = 1
    for {
      node <- ctx.classes.keys
      if visitedTime(node) == 0
    } yield {
      visit(null, node, time)
      time += 1
    }
  }

  private def createClassSymbols(implicit ctx: Context): Unit = {
    def create(clazz: ClassDef): Unit = {
      if (!ctx.global.contains(clazz.name)) {
        val symbol = clazz.parent match {
          case Some(Id(base)) =>
            create(ctx.classes(base))
            val baseSymbol = ctx.global(base)
            val typ = ClassType(clazz.name, Some(baseSymbol.typ))
            val scope = new ClassScope(Some(baseSymbol.scope))
            new ClassSymbol(clazz, typ, scope, Some(baseSymbol))
          case None =>
            val typ = ClassType(clazz.name)
            val scope = new ClassScope()
            new ClassSymbol(clazz, typ, scope)
        }
        ctx.global.declare(symbol)
      }
    }

    ctx.classes.values.foreach(create)
  }

  def resolveClasses(implicit ctx: Context): List[Named.ClassDef] = {
    val resolved = new mutable.HashMap[String, Named.ClassDef]

    def resolve(clazz: ClassDef): Unit = {
      if (!resolved.contains(clazz.name)) {
        val symbol = clazz.parent match {
          case Some(Id(base)) =>
            resolve(ctx.classes(base))
            ctx.global(clazz.name)
          case None =>
            ctx.global(clazz.name)
        }

        implicit val classCtx: ScopeContext = new ScopeContext(ctx.global).open(symbol.scope)
        val fs = clazz.fields.flatMap(resolveField)
        resolved(clazz.name) = Named.ClassDef(clazz.id, symbol.parent, fs)(symbol).setPos(clazz.pos)
      }
    }

    ctx.classes.values.foreach(resolve)
    ctx.classes.keys.map(resolved).toList
  }

  def resolveField(field: Field)(implicit ctx: ScopeContext): Option[Named.Field] = {
    val resolved = ctx.findConflict(field.name) match {
      case Some(earlier) =>
        (earlier, field) match {
          case (_: MemberVarSymbol, _: VarSymbol) =>
            issue(new OverridingVarError(field.name, field.pos));
            None
          case (suspect: MethodSymbol, m @ MethodDef(isStatic, returnType, id, params, body))
            if !suspect.isStatic && !m.isStatic =>
            // Only non-static methods can be overriden, but the type signature must be equivalent.
            val ret = typeTypeLit(returnType)
            ret.typ match {
              case NoType => None
              case retType =>
                val formalScope = new FormalScope
                val formalCtx = ctx.open(formalScope)
                val typedParams = params.flatMap { resolveLocalVarDef(_)(formalCtx, true) }
                val funType = FunType(typedParams.map(_.typeLit.typ), retType)
                if (suspect.typ === funType) { // override success
                  val symbol = new MethodSymbol(m, funType, typedParams.map(_.symbol), formalScope,
                    ctx.currentClass, Some(suspect))
                  ctx.declare(symbol)
                  Some(Named.MethodDef(isStatic, ret, id, typedParams, body)(symbol))
                } else { // override failure
                  issue(new BadOverrideError(m.name, suspect.owner.name, suspect.pos))
                  None
                }
            }
          case _ => issue(new DeclConflictError(field.name, earlier.pos, field.pos)); None
        }
      case None =>
        field match {
          case v @ VarDef(typeLit, id) =>
            val lit = typeTypeLit(typeLit)
            lit.typ match {
              case NoType => None
              case VoidType => issue(new BadVarTypeError(id.name, v.pos));
                None
              case t =>
                val symbol = new MemberVarSymbol(v, t, ctx.currentClass)
                ctx.declare(symbol)
                Some(Named.VarDef(lit, id)(symbol))
            }
          case m @ MethodDef(isStatic, returnType, id, params, body) =>
            val rt = typeTypeLit(returnType)
            rt.typ match {
              case NoType => None
              case retType =>
                val formalScope = new FormalScope
                val formalCtx: ScopeContext = ctx.open(formalScope)
                val typedParams = params.flatMap { resolveLocalVarDef(_)(formalCtx) }
                val funType = FunType(typedParams.map(_.typeLit.typ), retType)
                val symbol = new MethodSymbol(m, funType, typedParams.map(_.symbol), formalScope, ctx.currentClass)
                ctx.declare(symbol)
                Some(Named.MethodDef(isStatic, rt, id, typedParams, body)(symbol))
            }
        }
    }
    resolved.map(_.setPos(field.pos))
  }
}