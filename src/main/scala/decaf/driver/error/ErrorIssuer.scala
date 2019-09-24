package decaf.driver.error

import java.io.PrintStream

import scala.collection.mutable

/**
  * Decaf error issuer. The error must be a subclass of [[Error]].
  */
trait ErrorIssuer {

  /**
    * Issue/append an error.
    *
    * @param error error
    */
  def issue(error: Error): Unit = errors += error

  /**
    * Has any error been added?
    *
    * @return true if has error
    */
  def hasError: Boolean = errors.nonEmpty

  /**
    * Print out error messages, sorted by their error positions.
    *
    * @param to where to print
    */
  def printErrors(to: PrintStream = System.err): Unit = errors.sortBy(_.pos).foreach(to.println)

  private val errors = new mutable.ArrayBuffer[Error]
}
