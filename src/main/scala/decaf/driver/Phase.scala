package decaf.driver

import decaf.error.ErrorIssuer

abstract class Phase[In, Out](val name: String) extends ErrorIssuer {
  def transform(input: In): Out

  def apply(input: In): Option[Out] = {
    println(s"Running phase $name ...")
    val out = transform(input)
    printErrors()
    if (hasError) None else Some(out)
  }
}
