package decaf.driver

import java.io.File
import java.nio.file.Path

import scopt.Read
import scopt.Read.reads

object Config {

  abstract class Task(val name: String, val last: Phase.Value)

  case object TAC extends Task("tac", Phase.tac)

  case object JVM extends Task("jvm", Phase.jvm)

  case object TYPECHECK extends Task("typecheck", Phase.typer)

  case object PARSE extends Task("parse", Phase.parser)

  implicit val _ReadTask: Read[Task] = reads {
    case TAC.name => TAC
    case JVM.name => JVM
    case TYPECHECK.name => TYPECHECK
    case PARSE.name => PARSE
  }

  object Phase extends Enumeration {
    type Phase = Value
    val parser, namer, typer, tac, jvm = Value
  }

  implicit val _ReadPhase: Read[Phase.Value] = reads(Phase.withName)

  val PWD = new File(System.getProperty("user.dir"))
}

class Config(val sourceFile: File = Config.PWD,
             val task: Config.Task = Config.TAC,
             val outputDir: File = Config.PWD,
             val dumpPhases: Set[Config.Phase.Value] = Set()) {

  def getOutputPath(defaultSuffix: String): Path = {
    val name = sourceFile.toPath.getFileName.toString
    val base = if (name.endsWith(".decaf")) name.dropRight(".decaf".length) else name
    outputDir.toPath.resolve(base + defaultSuffix)
  }

  def needsOutput(phase: Config.Phase.Value): Boolean = (phase == task.last) || dumpPhases.contains(phase)

  def copy(source: File = this.sourceFile,
           task: Config.Task = this.task,
           output: File = this.outputDir,
           dumpPhases: Set[Config.Phase.Value] = this.dumpPhases): Config =
    new Config(source, task, output, dumpPhases)
}
