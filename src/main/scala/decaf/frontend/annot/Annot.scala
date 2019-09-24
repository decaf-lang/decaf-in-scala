package decaf.frontend.annot

/**
  * An ''annotation'' is something that is labeled on a tree node.
  */
trait Annot

/**
  * Anything that is labeled with an ''annotation''.
  */
trait Annotated[A <: Annot] {

  def annot: A
}