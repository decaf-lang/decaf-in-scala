package decaf.driver

import java.io._
import java.util.logging.Level

import decaf.driver.Config.Target
import decaf.lowlevel.log.Log
import scopt.{OptionParser, Read}

/**
  * Command line option parser.
  */
object OptParser extends OptionParser[Config]("decaf") {

  head("The Decaf compiler,", "version course-19-fall", "(scala)")

  arg[File]("file")
    .required()
    .text("input Decaf source")
    .action { case (f, config) => config.copy(source = f) }
    .validate { f =>
      if (!f.exists) {
        Left("not exist: " + f)
      } else if (!f.isFile) {
        Left("not a file: " + f)
      } else {
        Right()
      }
    }

  opt[File]('o', "output")
    .valueName("file")
    .text("output file for result, available except PA5 (default stdout)")
    .action { case (o, config) => config.copy(output = new FileOutputStream(o)) }
    .validate { f =>
      if (f.getParentFile != null && !f.getParentFile.exists) {
        Left("parent directory not exist: " + f.getParentFile)
      } else {
        Right()
      }
    }

  opt[File]('d', "dir")
    .valueName("directory")
    .text("output directory for low-level code, available >= PA3 (default .)")
    .action { case (d, config) => config.copy(dstDir = d) }
    .validate { d =>
      if (!d.exists) {
        Left("not exist: " + d)
      } else if (!d.isDirectory) {
        Left("not a directory: " + d)
      } else {
        Right()
      }
    }

  opt[Config.Target.Value]('t', "target")
    .valueName("target")
    .text("target/task: PA1, PA2, PA3, PA3-JVM, PA4, or PA5 (default)")
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
    .validate { f =>
      if (f.getParentFile != null && !f.getParentFile.exists) {
        Left("parent directory not exist: " + f.getParentFile)
      } else {
        Right()
      }
    }

  help('h', "help")
    .text("prints this usage text\n")

  checkConfig { config =>
    if (config.logLevel.intValue < Level.OFF.intValue) { // enable logging
      if (config.logFile == null) {
        Log.setup(config.logLevel, config.logColor)
      } else {
        Log.setup(config.logLevel, config.logColor, config.logFile.path)
      }
    }
    Right()
  }

  implicit private def ReadLogLevel: Read[Level] = Read.reads { s => Level.parse(s.toUpperCase) }

  implicit private def ReadTarget: Read[Target.Value] = Read.reads { s =>
    Target.withName(s.toUpperCase.replace("-", "_"))
  }
}
