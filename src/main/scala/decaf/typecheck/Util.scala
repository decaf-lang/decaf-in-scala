package decaf.typecheck

import decaf.annot.TypeImplicit._
import decaf.annot._
import decaf.error._
import decaf.tree.SyntaxTree._
import decaf.tree.{TypedTree => Typed}

trait Util extends ErrorIssuer {
  def typeTypeLit(typeLit: TypeLit)(implicit ctx: ScopeContext): Typed.TypeLit = {
    val typed = typeLit match {
      case TInt() => Typed.TInt()(IntType)
      case TBool() => Typed.TBool()(BoolType)
      case TString() => Typed.TString()(StringType)
      case TVoid() => Typed.TVoid()(VoidType)

      case TClass(id) =>
        ctx.lookupClass(id.name) match {
          case Some(clazz) => Typed.TClass(clazz)(clazz.typ)
          case None => issue(new ClassNotFoundError(id.name, typeLit.pos)); Typed.TVoid()(VoidType)
        }

      case TArray(elemType) =>
        val typedElemType = typeTypeLit(elemType)
        val typ = typedElemType.typ match {
          case NoType => NoType // avoid nested errors
          case VoidType => issue(new BadArrElementError(typeLit.pos)); NoType
          case t => ArrayType(t)
        }
        Typed.TArray(typedElemType)(typ)
    }
    typed.setPos(typeLit.pos)
  }
}
