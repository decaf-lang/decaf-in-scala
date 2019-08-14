package decaf.schedule

import decaf.error.ErrorIssuer

abstract class Phase[In, Out](val name: String) extends ErrorIssuer {
  def transform(input: In): Out

  def apply(input: In): Out = {
    val out = transform(input)
    printErrors()
    out
  }
}
