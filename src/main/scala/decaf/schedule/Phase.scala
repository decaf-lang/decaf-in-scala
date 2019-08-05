package decaf.schedule

import decaf.error.ErrorIssuer

abstract class Phase(val name: String) extends ErrorIssuer {
  type Input
  type Output

  def transform(input: Input): Output
}
