package decaf.annot

sealed trait Flag extends Annot

case object Yes extends Flag

case object No extends Flag

object FlagImplicit {

  implicit class FlagAnnotatedHasFlag(self: Annotated[Flag]) {
    def flag: Flag = self.annot

    def yes: Boolean = self.annot match {
      case Yes => true
      case No => false
    }

    def no: Boolean = self.annot match {
      case Yes => false
      case No => true
    }
  }

}
