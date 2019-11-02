package decaf.frontend.annot

/**
  * Types.
  *
  * Decaf has a very simple type system, consisting of:
  *
  *   - basic types: int, bool, and string
  *   - array types
  *   - class types
  *   - function types (cannot be directly expressed in programs, but we use them to type check function calls)
  *
  * Note the error type [[NoType]] is just a convenient placeholder for type checker and shall never be abused!
  *
  * @see [[BaseType]]
  * @see [[ClassType]]
  * @see [[ArrayType]]
  * @see [[FunType]]
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

  /** Check if two types are equivalent. */
  def ===(that: Type): Boolean = this == that

  /** Check if two types are not equivalent. */
  def !==(that: Type): Boolean = !(this === that)

  def noError: Boolean = true

  def isClassType: Boolean = false

  def isVoidType: Boolean = false

  def isBaseType: Boolean = false

  def isFuncType: Boolean = false
}

/**
  * Base types.
  */
class BaseType extends Type {

  override def isBaseType: Boolean = true

  override def <=(that: Type): Boolean = that match {
    case NoType => true
    case _ => this === that
  }
}

/**
  * A base type which is also JVM native. Currently, only int and bool are.
  */
trait JNativeType extends BaseType

/**
  * Integer type.
  */
object IntType extends JNativeType {

  override def toString: String = "int"
}

/**
  * Boolean type.
  */
object BoolType extends JNativeType {

  override def toString: String = "bool"
}

/**
  * String type.
  */
object StringType extends BaseType {

  override def toString: String = "string"
}

/**
  * Void type. Only possible as a function return type.
  */
object VoidType extends BaseType {

  override def isVoidType: Boolean = true

  override def toString: String = "void"
}

/**
  * Null type. Any {{{ null }}} literal will have this type.
  */
object NullType extends BaseType {

  override def <=(that: Type): Boolean = that match {
    case NoType | NullType | _: ClassType => true
    case _ => false
  }

  override def toString: String = "null"
}

/**
  * Class type.
  *
  * @param name   class name
  * @param parent type of super class (if any)
  */
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

/**
  * Array type.
  *
  * @param elemType element type
  */
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
    } else {
      s"$elemType[]"
    }
  }
}

/**
  * Function type.
  *
  * @param args type of arguments
  * @param ret  type of return value
  */
case class FunType(args: List[Type], ret: Type) extends Type {

  override def <=(that: Type): Boolean = that match {
    case NoType => true
    case FunType(params2, ret2) => (args.length == params2.length) &&
      (ret <= ret2) && (params2 zip args).forall { case (p2, p) => p2 <= p }
    case _ => false
  }

  override def ===(that: Type): Boolean = that match {
    case FunType(ts, t) => ret === t && args.length == ts.length && (args zip ts).forall {
      case (t1, t2) => t1 === t2
    }
    case _ => false
  }

  override def isFuncType: Boolean = true

  override def toString: String = {
    val ps = args match {
      case Nil => "()"
      case (t @ FunType(_, _)) :: Nil => s"($t)"
      case t :: Nil => t.toString
      case ts => s"(${ ts.mkString(", ") })"
    }
    s"$ps => $ret"
  }
}

/**
  * Representing an error type.
  */
object NoType extends Type {

  override def <=(that: Type): Boolean = true

  override def noError: Boolean = false

  override def toString: String = "Error"
}

object TypeImplicit {

  implicit class TypeAnnotatedHasType(self: Annotated[Type]) {

    /**
      * Access a node that is annotated with a [[Type]] by the field name `typ`.
      *
      * @example If `x` is annotated with a [[FunType]], then {{{ x.typ }}} gives you {{{ x.annot: FunType }}}.
      *
      * @return the annotation
      */
    def typ: Type = self.annot
  }

}