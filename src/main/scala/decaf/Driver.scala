package decaf

import decaf.frontend.error.Errors._
import scala.collection.mutable

object Driver {
  var errors: mutable.ListBuffer[Error] = new mutable.ListBuffer

  def err(error: Error): Unit = errors.addOne(error)
}
