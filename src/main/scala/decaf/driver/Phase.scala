package decaf.driver

import decaf.error.ErrorIssuer

abstract class Phase[In, Out](val name: String) extends ErrorIssuer {
  def transform(input: In): Out

  def post(output: Out)(implicit opt: Opt): Unit = {}

  def apply(input: In)(implicit opt: Opt): Option[Out] = {
    println(s"Running phase $name ...")
    val out = transform(input)
    printErrors()
    if (hasError) None else { post(out); Some(out) }
  }
}
