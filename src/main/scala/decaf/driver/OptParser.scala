package decaf.driver

import java.io._
import java.util.logging.Level

import decaf.driver.Config.Target
import scopt.{OptionParser, Read}

object OptParser extends OptionParser[Config]("decaf") {
  head("The decaf compiler", "v-19-fall", "(scala)")

  arg[File]("file")
    .required()
    .text("input decaf source")
    .action { case (f, config) => config.copy(source = f) }
    .validate { f =>
      if (!f.isFile) Left("not a file")
      else if (!f.exists) Left("not exist")
      else Right()
    }

  opt[File]('o', "output")
    .valueName("file")
    .text("output file for result, available except PA5 (default stdout)")
    .action { case (o, config) => config.copy(output = new FileOutputStream(o)) }
    .validate { f =>
      if (!f.isFile) Left("not a file")
      else if (!f.getParentFile.exists) Left("directory not exist")
      else Right()
    }

  opt[File]('d', "dir")
    .valueName("directory")
    .text("output directory for low-level code, available >= PA3 (default .)")
    .action { case (d, config) => config.copy(dstDir = d) }
    .validate { d =>
      if (!d.isDirectory) Left("not a directory")
      else if (!d.exists) Left("not exist")
      else Right()
    }

  opt[Config.Target.Value]('t', "target")
    .valueName("target")
    .text("target/task: PA1, PA2, PA3, PA3-JVM, PA4, or PA5")
    .action { case (t, config) => config.copy(target = t) }

  opt[Unit]("log-color")
    .text("enable colorful log (default plain)")
    .action { case (_, config) => config.copy(logColor = true) }

  opt[Level]("log-level")
    .valueName("level")
    .text("log level: all, severe, warning, info, config, fine, finer, finest, off (default)")
    .action { case (l, config) => config.copy(logLevel = l) }

  opt[File]("log-file")
    .valueName("file")
    .text("also dump log to a file")
    .action { case (f, config) => config.copy(logFile = f) }

  help('h', "help")
    .text("prints this usage text\n")

  checkConfig { config =>
    // TODO: setup logger
    Right()
  }

  implicit def ReadLogLevel: Read[Level] = Read.reads(Level.parse)

  implicit def ReadTarget: Read[Target.Value] = Read.reads {
    case "PA3-JVM" => Target.PA3_JVM
    case other => Target.withName(other)
  }
}
