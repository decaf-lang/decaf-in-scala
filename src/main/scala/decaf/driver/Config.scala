package decaf.driver

import java.io.{FileInputStream, InputStream, OutputStream}
import java.util.logging.Level

import scala.reflect.io.Path

/**
  * Compiler configuration.
  *
  * @param source   input Decaf source
  * @param output   result output (<= PA3)
  * @param dstDir   output directory for TAC and assembly code (>= PA3)
  * @param target   target/task (see [[decaf.driver.Config.Target]])
  * @param logColor enable log color
  * @param logLevel log level (see [[Level]])
  * @param logFile  also dump log to file
  */
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

  /**
    * Get input Decaf source stream.
    */
  def sourceStream: InputStream = new FileInputStream(source.jfile)

  /**
    * Get just the base name, without extension, of the input Decaf source.
    *
    * @return base name
    *
    * @example the base name of `myFolder/blackjack.decaf` is `blackjack`
    **/
  def sourceBaseName: String = source.stripExtension
}

object Config {

  /**
    * Current directory.
    */
  val PWD: Path = System.getProperty("user.dir")

  /**
    * Standard output.
    */
  val STDOUT: OutputStream = System.out

  /**
    * Target/task. Options: PA1, PA2, PA3, PA3-JVM, PA4, PA5.
    */
  object Target extends Enumeration {

    type Target = Value
    val PA1, PA2, PA3, PA3_JVM, PA4, PA5 = Value
  }

}
