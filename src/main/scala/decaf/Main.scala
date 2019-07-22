package decaf

import java.io.FileReader

import decaf.frontend.parsing.Parser

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: decaf.jar file")
      return
    }

    val reader = new FileReader(args.head)
    val parser = new Parser
    parser.parse(reader) match {
      case parser.Success(r, _) => println(r)
      case parser.Failure(msg, next) => println(s"${next.pos}:$msg:\n${next.pos.longString}")
      case parser.Error(msg, next) => println(s"${next.pos}:$msg:\n${next.pos.longString}")
    }
  }
}
