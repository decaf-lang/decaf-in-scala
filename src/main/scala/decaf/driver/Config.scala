package decaf.driver

import java.io._
import java.nio.file.Path

import scopt.Read
import scopt.Read.reads

object Config {

  object Target extends Enumeration {
    type Target = Value
    val jvm, PA1, PA2, PA3 = Value
  }

  implicit val _ReadTarget: Read[Target.Value] = reads(Target.withName)

  val PWD = new File(System.getProperty("user.dir")).toPath
  val STDOUT = System.out
}

class Config(val source: InputStream = null,
             val outputStream: PrintStream = Config.STDOUT,
             val outputDir: Path = Config.PWD,
             val target: Config.Target.Value = Config.Target.jvm) {

  def copy(source: InputStream = this.source,
           outputStream: PrintStream = this.outputStream,
           outputDir: Path = this.outputDir,
           target: Config.Target.Value = this.target): Config =
    new Config(source, outputStream, outputDir, target)
}
