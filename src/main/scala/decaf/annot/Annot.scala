package decaf.annot

trait Annot

trait Annotated[A <: Annot] {
  val annot: A
}