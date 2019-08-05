package decaf.frontend.typecheck

import decaf.error._
import decaf.frontend.annot.ResolvedImplicit._
import decaf.frontend.annot.TypedImplicit._
import decaf.frontend.annot._
import decaf.frontend.tree.TreeNode._
import decaf.frontend.tree.{ResolvedTree, SyntaxTree, TreeNode}
import decaf.schedule.Phase

import scala.collection.mutable.HashMap

class Resolver extends Phase("resolver") with TypeLitResolver {
  override type Input = SyntaxTree.Tree
  override type Output = ResolvedTree.Tree

  override def transform(tree: SyntaxTree.Tree): ResolvedTree.Tree = {
    val classes = tree.classes
    val mapping = new HashMap[String, SyntaxTree.ClassDef]

    // Check conflicting definitions. If any, ignore the redefined ones.
    classes.foreach { clazz =>
      mapping.get(clazz.name) match {
        case Some(earlier) => issue(new DeclConflictError(clazz.name, earlier.pos, clazz.pos))
        case None => mapping(clazz.name) = clazz
      }
    }

    // Make sure the base class exists. If not, ignore the inheritance.
    mapping.foreach { case (name, clazz) =>
      clazz.parent match {
        case Some(Id(base)) if !mapping.contains(base) =>
          issue(new ClassNotFoundError(base, clazz.pos))
          mapping(name) = clazz.parentDetached
        case _ =>
      }
    }

    // Make sure any inheritance does not form a cycle. If so, eliminate the cycle by cutting an edge on the cycle.
    // Example: suppose A extends B, B extends C and C extends A, then we see A -> B -> C -> A forms a cycle.
    // We cut the last edge C -> A by detaching C's parent.
    eliminateCycles(mapping)

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
    val globalScope = new GlobalScope
    createClassSymbols(mapping, globalScope)

    // Now, we can resolve every class definition to fill in its class scope table. To check if the overriding
    // behaves correctly, we should first resolve a base class and then its subclasses.
    val resolvedClasses = resolveClassFields(mapping, globalScope)

    // Finally, let's locate the main class, whose name is 'Main', and contains a method like:
    //  static void main() { ... }
    resolvedClasses.find(_.name == "Main") match {
      case Some(clazz) =>
        clazz.symbol.scope.lookup("main") match {
          case Some(symbol) =>
            symbol match {
              case f: FunSymbol if f.isMain => // ok
              case _ => issue(NoMainClassError)
            }
          case _ => issue(NoMainClassError)
        }
      case None => issue(NoMainClassError)
    }

    TopLevel(resolvedClasses, globalScope).setPos(tree.pos)
  }

  private def eliminateCycles(mapping: HashMap[String, SyntaxTree.ClassDef]): Unit = {
    val visitedTime = new HashMap[String, Int]
    mapping.keys.foreach(visitedTime(_) = 0)

    @scala.annotation.tailrec
    def visit(from: SyntaxTree.ClassDef, node: String, time: Int): Unit = {
      if (visitedTime(node) == 0) { // not visited yet
        visitedTime(node) = time
        val clazz = mapping(node)
        clazz.parent match {
          case Some(Id(base)) => visit(clazz, base, time)
          case _ => // done
        }
      } else if (visitedTime(node) == time) { // find a cycle
        issue(new BadInheritanceError(from.pos))
        mapping(from.name) = from.parentDetached
      } // else: this node is visited earlier, also done
    }

    var time = 1
    for {
      node <- mapping.keys
      if visitedTime(node) == 0
    } yield {
      visit(null, node, time)
      time += 1
    }
  }

  private def createClassSymbols(mapping: HashMap[String, SyntaxTree.ClassDef], created: GlobalScope): Unit = {
    def create(clazz: SyntaxTree.ClassDef): Unit = {
      if (!created.contains(clazz.name)) {
        val symbol = clazz.parent match {
          case Some(Id(base)) =>
            create(mapping(base))
            val baseSymbol = created(base)
            val typ = ClassType(clazz.name, Some(baseSymbol.typ))
            val scope = new ClassScope(Some(baseSymbol.scope))
            new ClassSymbol(clazz, typ, scope)
          case None =>
            val typ = ClassType(clazz.name)
            val scope = new ClassScope()
            new ClassSymbol(clazz, typ, scope)
        }
        created.declare(symbol)
      }
    }

    mapping.values.foreach(create)
  }

  def resolveClassFields(mapping: HashMap[String, SyntaxTree.ClassDef],
                         globalScope: GlobalScope): List[ResolvedTree.ClassDef] = {
    val resolved = new HashMap[String, ResolvedTree.ClassDef]

    def resolve(clazz: SyntaxTree.ClassDef): Unit = {
      if (!resolved.contains(clazz.name)) {
        val symbol: ClassSymbol = clazz.parent match {
          case Some(Id(base)) =>
            resolve(mapping(base))
            globalScope(clazz.name)
          case None =>
            globalScope(clazz.name)
        }
        val ctx = new ScopeContext(globalScope).open(symbol.scope)
        val resolvedFields: List[ResolvedTree.Field] = clazz.fields.flatMap {
          case f: SyntaxTree.VarDef => resolveVarDef(f, ctx)
          case f: SyntaxTree.MethodDef => resolveMethodDef(f, ctx)
        }
        resolved(clazz.name) = ClassDef(clazz.id, clazz.parent, resolvedFields, symbol).setPos(clazz.pos)
      }
    }

    mapping.values.foreach(resolve)
    mapping.keys.map(resolved).toList
  }

  def resolveVarDef(varDef: SyntaxTree.VarDef, ctx: ScopeContext): Option[ResolvedTree.VarDef] =
  // Variables can never be overriden, thus the identifier cannot conflict with any already defined symbol.
    ctx.search(varDef.name) match {
      case Some(earlier) =>
        earlier match {
          case _: VarSymbol => // override failure
            issue(new OverridingVarError(varDef.name, varDef.pos))
          case _ => issue(new DeclConflictError(varDef.name, earlier.pos, varDef.pos))
        }
        None
      case None =>
        val SyntaxTree.VarDef(typeLit, id) = varDef
        val typedTypeLit = resolveTypeLit(typeLit, ctx)
        typedTypeLit.typ match {
          case NoType => None
          case VoidType => issue(new BadVarTypeError(id.name, varDef.pos)); None
          case t =>
            val symbol = new VarSymbol(varDef, t)
            ctx.declare(symbol)
            Some(ResolvedTree.VarDef(typedTypeLit, id, symbol).setPos(varDef.pos))
        }
    }

  def resolveMethodDef(methodDef: SyntaxTree.MethodDef, ctx: ScopeContext): Option[ResolvedTree.MethodDef] =
  // Only non-static methods can be overriden, but the type signature must be equivalent.
    ctx.search(methodDef.name) match {
      case Some(earlier) =>
        earlier match {
          case suspect: FunSymbol if !suspect.isStatic && !methodDef.isStatic =>
            // maybe override
            val SyntaxTree.MethodDef(isStatic, returnType, id, params, body) = methodDef
            val typedReturnType = resolveTypeLit(returnType, ctx)
            typedReturnType.typ match {
              case NoType => None
              case retType =>
                val formalScope = new FormalScope
                val typedParams = params.flatMap(resolveVarDef(_, ctx.open(formalScope)))
                val funType = FunType(typedParams.map(_.typeLit.typ), retType)
                if (suspect.typ eq funType) { // override success
                  val symbol = new FunSymbol(methodDef, funType, typedParams.map(_.symbol), formalScope, ctx.currentClass)
                  ctx.declare(symbol)
                  Some(ResolvedTree.MethodDef(isStatic, typedReturnType, id, typedParams, body).setPos(methodDef.pos))
                } else { // override failure
                  issue(new BadOverrideError(methodDef.name, suspect.parent.name, suspect.pos))
                  None
                }
            }
          case _ =>
            issue(new DeclConflictError(methodDef.name, earlier.pos, methodDef.pos))
            None
        }
      case None =>
        val SyntaxTree.MethodDef(isStatic, returnType, id, params, body) = methodDef
        val typedReturnType = resolveTypeLit(returnType, ctx)
        typedReturnType.typ match {
          case NoType => None
          case retType =>
            val formalScope = new FormalScope
            val typedParams = params.flatMap(resolveVarDef(_, ctx.open(formalScope)))
            val funType = FunType(typedParams.map(_.typeLit.typ), retType)
            val symbol = new FunSymbol(methodDef, funType, typedParams.map(_.symbol), formalScope, ctx.currentClass)
            ctx.declare(symbol)
            Some(ResolvedTree.MethodDef(isStatic, typedReturnType, id, typedParams, body).setPos(methodDef.pos))
        }
    }
}
