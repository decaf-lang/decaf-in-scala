package decaf.frontend.typecheck

import decaf.error.{BadArrElementError, ClassNotFoundError, ErrorIssuer}
import decaf.frontend.annot.TypedImplicit._
import decaf.frontend.annot._
import decaf.frontend.tree.SyntaxTree._
import decaf.frontend.tree.{TypedTree => Typed}

trait TypeLitResolver extends ErrorIssuer {
  def resolveTypeLit(typeLit: TypeLit, ctx: ScopeContext): Typed.TypeLit = {
    val typed = typeLit match {
      case TInt() => Typed.TInt()(IntType)
      case TBool() => Typed.TBool()(BoolType)
      case TString() => Typed.TString()(StringType)
      case TVoid() => Typed.TVoid()(VoidType)

      case TClass(id) =>
        val typ = ctx.lookupClass(id.name) match {
          case Some(symbol) => symbol.typ
          case None => issue(new ClassNotFoundError(id.name, typeLit.pos)); NoType
        }
        Typed.TClass(id)(typ)

      case TArray(elemType) =>
        val typedElemType = resolveTypeLit(elemType, ctx)
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
