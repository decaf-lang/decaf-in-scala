package decaf.error

import scala.collection.mutable

trait ErrorIssuer {
  private val errors: mutable.ArrayBuffer[Error] = new mutable.ArrayBuffer

  def issue(error: Error): Unit = errors.addOne(error)
}
