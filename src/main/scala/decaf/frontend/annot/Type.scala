package decaf.frontend.annot

sealed trait Type extends Annot {
  /**
    * Check if `this` is a subtype of `that`, denoted by `this <: that`.
    * Rules:
    * {{{
    *   t <: t (reflexive)
    *   t1 <: t3 if t1 <: t2 and t2 <: t3 (transitive)
    *   Error <: t, t <: Error
    *   null <: class c
    *   class c1 <: class c2 if c1 extends c2
    *   (t1, t2, ..., tn) -> t <: (s1, s2, ..., sn) -> s if t <: s and si <: ti for every i
    * }}}
    */
  def sub(that: Type): Boolean

  def eq(that: Type): Boolean = this == that
}

object NoType extends Type {
  override def sub(that: Type): Boolean = true

  override def toString: String = "Error"
}

class BaseType extends Type {
  override def sub(that: Type): Boolean = that match {
    case NoType => true
    case _ => this eq that
  }
}

object IntType extends BaseType {
  override def toString: String = "int"
}

object BoolType extends BaseType {
  override def toString: String = "bool"
}

object StringType extends BaseType {
  override def toString: String = "string"
}

object VoidType extends BaseType {
  override def toString: String = "void"
}

object NullType extends BaseType {
  override def sub(that: Type): Boolean = that match {
    case NoType | NullType | _: ClassType => true
    case _ => false
  }

  override def toString: String = "null"
}

case class ClassType(name: String, parent: Option[ClassType] = None) extends Type {
  override def sub(that: Type): Boolean = that match {
    case NoType => true
    case ClassType(_, None) => this eq that
    case ClassType(_, Some(t)) => (this eq that) || (this sub t)
    case _ => false
  }

  override def eq(that: Type): Boolean = that match {
    case ClassType(n, _) if name == n => true
    case _ => false
  }

  override def toString: String = s"class : $name"
}

case class ArrayType(elemType: Type) extends Type {
  override def sub(that: Type): Boolean = that match {
    case NoType => true
    case _ => this eq that
  }

  override def toString: String = s"$elemType[]"
}

case class FunType(params: List[Type], ret: Type) extends Type {
  override def sub(that: Type): Boolean = that match {
    case NoType => true
    case FunType(params2, ret2) => (params.length == params2.length) &&
      (ret sub ret2) && (params2 zip params).forall { case (p2, p) => p2 sub p }
    case _ => ???
  }

  override def toString: String = params.map(_ + "->").mkString + ret
}