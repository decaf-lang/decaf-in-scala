package decaf.driver

import java.io.{FileInputStream, InputStream, OutputStream}
import java.util.logging.Level

import scala.reflect.io.Path

object Config {

  object Target extends Enumeration {
    type Target = Value
    val PA1, PA2, PA3, PA3_JVM, PA4, PA5 = Value
  }

  val PWD: Path = System.getProperty("user.dir")
  val STDOUT: OutputStream = System.out
}

class Config(val source: Path = Config.PWD,
             val output: OutputStream = Config.STDOUT,
             val dstDir: Path = Config.PWD,
             val target: Config.Target.Value = Config.Target.PA5,
             val logColor: Boolean = false,
             val logLevel: Level = Level.OFF,
             val logFile: Path = null) {

  def copy(source: Path = this.source,
           output: OutputStream = this.output,
           dstDir: Path = this.dstDir,
           target: Config.Target.Value = this.target,
           logColor: Boolean = this.logColor,
           logLevel: Level = this.logLevel,
           logFile: Path = this.logFile): Config =
    new Config(source, output, dstDir, target, logColor, logLevel, logFile)

  def sourceStream: InputStream = new FileInputStream(source.jfile)

  /**
    * Get just the base name, without extension, of the input Decaf source.
    *
    * Example: the base name of `myFolder/blackjack.decaf`` is `blackjack`.
    *
    * @return base name
    **/
  def sourceBaseName: String = source.stripExtension
}
