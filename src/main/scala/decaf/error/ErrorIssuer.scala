package decaf.error

import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable

trait ErrorIssuer extends StrictLogging {
  private val errors: mutable.ArrayBuffer[Error] = new mutable.ArrayBuffer

  def issue(error: Error): Unit = errors.addOne(error)

  def hasError: Boolean = errors.nonEmpty

  def printErrors(): Unit = {
    errors.foreach { error =>
      Console.err.println(error)
      logger.info("Issued: {}", error)
    }
  }
}
