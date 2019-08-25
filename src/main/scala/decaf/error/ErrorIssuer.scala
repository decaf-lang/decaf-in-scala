package decaf.error

import java.io.PrintStream

import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable

trait ErrorIssuer extends StrictLogging {
  private val errors: mutable.ArrayBuffer[Error] = new mutable.ArrayBuffer

  def issue(error: Error): Unit = errors.addOne(error)

  def hasError: Boolean = errors.nonEmpty

  def printErrors(to: PrintStream = System.err): Unit = errors.sortBy(_.pos).foreach(to.println)
}
