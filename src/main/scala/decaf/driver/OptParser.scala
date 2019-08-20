package decaf.driver

import java.io.File

import decaf.driver.Config.Task
import scopt.OptionParser

object OptParser extends OptionParser[Config]("decaf") {
  head("The decaf compiler", "(scala version)")

  arg[File]("file")
    .required()
    .text("input decaf source")
    .action { case (f, config) => config.copy(source = f) }

  opt[Task]('t', "task")
    .valueName("task")
    .text("compilation target/task, default tac")
    .action { case (t, config) => config.copy(task = t) }

  opt[File]('o', "output")
    .valueName("folder")
    .action { case (o, config) => config.copy(output = o) }
    .text("output folder, default present folder (./)")

  help('h', "help")
    .text("prints this usage text\n")

  // TODO
  cmd("debug")
    .children(
      opt[Seq[Config.Phase.Value]]('d', "dump")
        .valueName("p1,p2,...")
        .text("dump intermediate results of phases p1, p2, ... to output folder, all if no phase is given"),

      opt[Int]('l', "log-output")
        .valueName("path")
        .text("log output file, default stdout"),

      opt[Unit]('f', "force")
        .text("overwrite the log output file if exists"),

      opt[Int]('L', "log-level")
        .valueName("level")
        .text("log level"),

      opt[Unit]("log-color")
        .text("colorful log")
    )
}
