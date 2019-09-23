package decaf.frontend.annot

/**
  * Types.
  *
  * Decaf has a very simple type system, consisting of:
  * - basic types: int, bool, and string
  * - array types
  * - class types
  * - function types (cannot be expressed in programs, but we use them to type check function calls)
  */
sealed trait Type extends Annot {
  /**
    * Check if `this` is a subtype of `that`, denoted by `this <= that`.
    * Rules:
    * {{{
    *   t <= t (reflexive)
    *   t1 <= t3 if t1 <= t2 and t2 <= t3 (transitive)
    *   Error <= t, t <= Error
    *   null <= class c
    *   class c1 <= class c2 if c1 extends c2
    *   (t1, t2, ..., tn) -> t <= (s1, s2, ..., sn) -> s if t <= s and si <= ti for every i
    * }}}
    */
  def <=(that: Type): Boolean

  def ===(that: Type): Boolean = this == that

  def !==(that: Type): Boolean = !(this === that)

  def noError: Boolean = true

  def isClassType: Boolean = false

  def isVoidType: Boolean = false

  def isBaseType: Boolean = false

  def isFuncType: Boolean = false
}

class BaseType extends Type {
  override def isBaseType: Boolean = true

  override def <=(that: Type): Boolean = that match {
    case NoType => true
    case _ => this === that
  }
}

trait JNativeType extends BaseType

object IntType extends JNativeType {
  override def toString: String = "int"
}

object BoolType extends JNativeType {
  override def toString: String = "bool"
}

object StringType extends BaseType {
  override def toString: String = "string"
}

object VoidType extends BaseType {
  override def isVoidType: Boolean = true

  override def toString: String = "void"
}

object NullType extends BaseType {
  override def <=(that: Type): Boolean = that match {
    case NoType | NullType | _: ClassType => true
    case _ => false
  }

  override def toString: String = "null"
}

case class ClassType(name: String, parent: Option[ClassType] = None) extends Type {
  override def <=(that: Type): Boolean = that match {
    case NoType => true
    case _: ClassType => parent match {
      case Some(base) => (this === that) || (base <= that)
      case None => this === that
    }
    case _ => false
  }

  override def ===(that: Type): Boolean = that match {
    case ClassType(n, _) if name == n => true
    case _ => false
  }

  override def isClassType: Boolean = true

  override def toString: String = s"class $name"
}

case class ArrayType(elemType: Type) extends Type {
  override def <=(that: Type): Boolean = that match {
    case NoType => true
    case _ => this === that
  }

  override def ===(that: Type): Boolean = that match {
    case ArrayType(t) => elemType === t
    case _ => false
  }

  override def toString: String = {
    if (elemType.isFuncType) {
      // NOTE: [] has higher priority than functions, so we must add extra parenthesis, e.g.
      // `(int => int)[]` means an array of functions from integers to integers, but
      // `int => int[]` means a function from integers to integer arrays
      s"($elemType)[]"
    } else s"$elemType[]"
  }
}

case class FunType(params: List[Type], ret: Type) extends Type {
  override def <=(that: Type): Boolean = that match {
    case NoType => true
    case FunType(params2, ret2) => (params.length == params2.length) &&
      (ret <= ret2) && (params2 zip params).forall { case (p2, p) => p2 <= p }
    case _ => false
  }

  override def ===(that: Type): Boolean = that match {
    case FunType(ts, t) => ret === t && params.length == ts.length && (params zip ts).forall {
      case (t1, t2) => t1 === t2
    }
    case _ => false
  }

  override def isFuncType: Boolean = true

  override def toString: String = {
    val ps = params match {
      case Nil => "()"
      case t :: Nil => t.toString
      case ts => s"(${ ts.mkString(", ") })"
    }
    s"$ps => $ret"
  }
}

object NoType extends Type {
  override def <=(that: Type): Boolean = true

  override def noError: Boolean = false

  override def toString: String = "Error"
}

object TypeImplicit {

  implicit class TypeAnnotatedHasType(self: Annotated[Type]) {
    def typ: Type = self.annot
  }

}