package decaf.annot

/**
  * An "annotation" is something that is labeled on a tree node.
  */
trait Annot

/**
  * Anything that is labeled with an annotation. In decaf, all tree nodes are annotated with some annotation `A`.
  */
trait Annotated[A <: Annot] {
  val annot: A
}