package decaf.driver

/**
  * Represents a "task" function that accepts one argument and may produce a result. Can be regarded as a "partial"
  * function.
  *
  * @note our requirement differs from [[scala.PartialFunction]], so we create our own.
  */
trait Task[-T, +U] extends Function[T, Option[U]] {

  /**
    * Pipe two tasks and returns a function which does "this" first, if succeeds, continue do "next" with
    * the previous result as input; or else exits and returns [[None]].
    *
    * In terms of monad, this is just a Kleisli composition.
    *
    * @param g "next" function
    * @return the piped (Kleisli-composed) function
    */
  def |>[R](g: Task[U, R]): Task[T, R] = x => this (x) flatMap g
}
