package decaf.driver

import java.io.{File, FileInputStream, InputStream, PrintStream}
import java.nio.file.Path

import scopt.Read.reads
import scopt.{OptionParser, Read}

object OptParser extends OptionParser[Config]("decaf") with NativeLogger with OptImplicits {
  head("The decaf compiler", "(scala version)")

  arg[InputStream]("file")
    .required()
    .text("input decaf source")
    .action { case (f, config) => config.copy(source = f) }

  opt[PrintStream]('o', "output")
    .valueName("file")
    .action { case (o, config) => config.copy(outputStream = o) }
    .text("output file (default stdout)")

  opt[Path]('d', "dir")
    .valueName("directory")
    .action { case (d, config) => config.copy(outputDir = d) }
    .text("output directory (enabled when target=jvm, default .)")

  opt[Config.Target.Value]('t', "target")
    .valueName("target")
    .text("compilation target: jvm (default), PA1, PA2, or PA3")
    .action { case (t, config) => config.copy(target = t) }

  help('h', "help")
    .text("prints this usage text\n")

  //  cmd("debug")
  //    .children(
  //      opt[Seq[Config.Target.Value]]('d', "dump")
  //        .valueName("p1,p2,...")
  //        .text("dump intermediate results of phases p1, p2, ... to output folder, all if no phase is given")
  //        .action { case (ps, config) => config.copy(dumpPhases = config.dumpPhases ++ ps) },
  //
  //      opt[Level]('L', "log-level")
  //        .valueName("level")
  //        .text("log level")
  //        .action { case (level, config) => setLevel(level); config }
  //    )
}

trait OptImplicits {

  implicit val _ReadInputStream: Read[InputStream] = reads { path =>
    val file = new File(path)
    if (file.isDirectory)
      throw new IllegalArgumentException(s"Expected a file, but '$path' is a directory.")
    else if (!file.exists)
      throw new IllegalArgumentException(s"File '$path' does not exist.")
    else if (!path.endsWith(".decaf"))
      throw new IllegalArgumentException(s"'$path' is not a decaf source.")
    else new FileInputStream(file)
  }

  implicit val _ReadPrintStream: Read[PrintStream] = reads { path =>
    val file = new File(path)
    if (file.isDirectory && !file.exists)
      throw new IllegalArgumentException(s"Directory '$path' does not exist.")
    else if (file.isFile && !file.getParentFile.exists)
      throw new IllegalArgumentException(s"The parent directory of '$path' does not exist.")
    else new PrintStream(file)
  }

  implicit val _ReadPath: Read[Path] = reads { path =>
    val file = new File(path)
    if (file.isFile)
      throw new IllegalArgumentException(s"'$path' is not a directory.")
    else if (!file.exists)
      throw new IllegalArgumentException(s"Directory '$path' does not exist.")
    else file.toPath
  }

}
