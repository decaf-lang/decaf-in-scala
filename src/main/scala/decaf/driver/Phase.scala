package decaf.driver

import java.io.PrintStream

import decaf.error.ErrorIssuer

abstract class Phase[In, Out](val name: String) extends ErrorIssuer {
  def transform(input: In): Out

  def post(output: Out)(implicit config: Config): Unit = {}

  def apply(input: In)(implicit config: Config): Option[Out] = {
    val out = transform(input)
    if (hasError) {
      printErrors(System.err)
      if (config.output != Config.STDOUT && config.target <= Config.Target.PA3) {
        printErrors(new PrintStream(config.output))
      }
      None
    } else {
      post(out)
      Some(out)
    }
  }
}
