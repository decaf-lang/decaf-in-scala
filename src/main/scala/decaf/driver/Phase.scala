package decaf.driver

import decaf.error.ErrorIssuer

abstract class Phase[In, Out](val name: String) extends ErrorIssuer {
  def transform(input: In): Out

  def post(output: Out)(implicit config: Config): Unit = {}

  def apply(input: In)(implicit config: Config): Option[Out] = {
    //    println(s"Running phase $name ...")
    val out = transform(input)
    printErrors()
    if (config.target >= Config.Target.PA1) printErrors(config.outputStream)
    if (hasError) None else { post(out); Some(out) }
  }
}
