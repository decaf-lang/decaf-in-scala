package decaf.frontend.typecheck

import decaf.error.{BadArrElementError, ClassNotFoundError, ErrorIssuer}
import decaf.frontend.annot.TypedImplicit._
import decaf.frontend.annot._
import decaf.frontend.tree.TreeNode._
import decaf.frontend.tree.{SyntaxTree, TypedTree}

trait TypeLitResolver extends ErrorIssuer {
  def resolveTypeLit(typeLit: SyntaxTree.TypeLit, ctx: ScopeContext): TypedTree.TypeLit = typeLit match {
    case TInt(_) => TInt(IntType)
    case TBool(_) => TBool(BoolType)
    case TString(_) => TString(StringType)
    case TVoid(_) => TVoid(VoidType)

    case TClass(id, _) =>
      val typ = ctx.lookupClass(id.name) match {
        case Some(symbol) => symbol.typ
        case None => issue(new ClassNotFoundError(id.name, typeLit.pos)); NoType
      }
      TClass(id, typ)

    case TArray(elemType, _) =>
      val typedElemType = resolveTypeLit(elemType, ctx)
      val typ = typedElemType.typ match {
        case NoType => NoType // avoid nested errors
        case VoidType => issue(new BadArrElementError(typeLit.pos)); NoType
        case t => ArrayType(t)
      }
      TArray(typedElemType, typ)
  }
}
