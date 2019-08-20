package decaf.driver

import java.io.FileReader

object Launcher {
  def withArgs(args: Array[String]): Unit = {
    OptParser.parse(args, new Config) match {
      case Some(config) => withConfig(config)
      case None =>
    }
  }

  def withConfig(implicit config: Config): Unit = {
    val reader = new FileReader(config.sourceFile)
    val tasks = new Tasks
    val task = config.task match {
      case Config.TAC => tasks.tac
      case Config.JVM => tasks.jvm
      case Config.TYPECHECK => tasks.typeCheck
      case Config.PARSE => tasks.parse
    }
    task.run(reader)
  }
}
