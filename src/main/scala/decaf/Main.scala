package decaf

import java.io.FileReader

import decaf.driver.Tasks

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: decaf.jar file")
      return
    }

    val reader = new FileReader(args.head)
    Tasks.tac.run(reader)
  }
}
