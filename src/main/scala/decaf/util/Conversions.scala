package decaf.util

import scala.jdk.CollectionConverters._

/**
  * A group of common conversions between Java and Scala.
  */
object Conversions {

  implicit def ScalaListToJavaList[T](list: List[T]): java.util.List[T] = list.asJava

  implicit def JavaListToScalaList[T](list: java.util.List[T]): List[T] = list.asScala.toList

  implicit def ScalaOptionToJavaOptional[T](option: Option[T]): java.util.Optional[T] = option match {
    case Some(value) => java.util.Optional.of(value)
    case None => java.util.Optional.empty()
  }

  implicit def JavaOptionalToScalaOption[T](optional: java.util.Optional[T]): Option[T] =
    if (optional.isEmpty) None else Some(optional.get)
}
