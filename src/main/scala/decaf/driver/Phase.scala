package decaf.driver

import java.io.PrintStream

import decaf.driver.error.ErrorIssuer

/**
  * Compilation of a Decaf program is processed phase-by-phase. Each phase transforms a kind of language representation
  * into another.
  *
  * @tparam In  type of input
  * @tparam Out type of output
  * @param name phase name
  * @see [[decaf.driver.Tasks]]
  * @see [[ErrorIssuer]]
  */
abstract class Phase[In, Out](val name: String) extends ErrorIssuer {
  /**
    * Entry of the actual transformation.
    *
    * @param input input
    * @return output
    */
  def transform(input: In): Out

  /**
    * A phase is said to be ''successful'', if and only if no errors occur (i.e. `!hasError()`).
    * When a phase is successful, this method will be executed.
    *
    * @param output output of the transformation
    */
  def onSucceed(output: Out)(implicit config: Config): Unit = {}

  /**
    * Entry of the phase.
    *
    * @param input input
    * @return output (if succeeds)
    */
  def apply(input: In)(implicit config: Config): Option[Out] = {
    val out = transform(input)
    if (hasError) {
      printErrors(System.err)
      if (config.output != Config.STDOUT && config.target <= Config.Target.PA3) {
        printErrors(new PrintStream(config.output))
      }
      None
    } else {
      onSucceed(out)
      Some(out)
    }
  }
}
