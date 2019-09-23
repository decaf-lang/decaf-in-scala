package decaf.backend.opt

import java.io.PrintWriter

import decaf.driver.{Config, Phase}
import decaf.lowlevel.tac.{Simulator, TacProg}

/**
  * TAC optimization phase: optimize a TAC program.
  *
  * The original decaf compiler has NO optimization, thus, we implement the transformation as identity function.
  */
class Optimizer extends Phase[TacProg, TacProg]("optimizer") {

  override def transform(input: TacProg): TacProg = input

  override def post(program: TacProg)(implicit config: Config): Unit = {
    if (config.target.equals(Config.Target.PA4)) { // First dump the tac program to file,
      val path = config.dstDir / config.sourceBaseName + ".tac"
      val printer = new PrintWriter(path)
      program.printTo(printer)
      printer.close()

      // and then execute it using our simulator.
      val simulator = new Simulator(System.in, config.output)
      simulator.execute(program)
    }
  }
}
