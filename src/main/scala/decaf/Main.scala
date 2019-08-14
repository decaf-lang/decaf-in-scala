package decaf

import java.io.FileReader

import decaf.driver.{Opt, Tasks}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: decaf.jar file")
      return
    }

    val fileName = args.head
    implicit val opt = new Opt(fileName)
    val reader = new FileReader(fileName)
    val tasks = new Tasks
    tasks.tac.run(reader)
  }
}
