package decaf.driver

import ch.qos.logback.classic.{Level, Logger}
import org.slf4j.LoggerFactory
import scopt.Read
import scopt.Read.reads

// FIXME: use an easier logger in future
trait NativeLogger {
  val LOGGER = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]

  type Level = ch.qos.logback.classic.Level

  implicit val _ReadLevel: Read[Level] = reads {
    case "off" => Level.OFF
    case "error" => Level.ERROR
    case "warn" => Level.WARN
    case "info" => Level.INFO
    case "debug" => Level.DEBUG
    case "trace" => Level.TRACE
    case "all" => Level.ALL
  }

  def setLevel(level: Level): Unit = LOGGER.setLevel(level)

  setLevel(Level.OFF)
}
