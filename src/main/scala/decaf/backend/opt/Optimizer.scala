package decaf.backend.opt

import java.io.PrintWriter

import decaf.driver.{Config, Phase}
import decaf.lowlevel.tac.{Simulator, TacProg}

/**
  * TAC optimization phase: optimize a TAC program.
  *
  * NO optimization is done here, as you see we implemented the transformation as an identity function -- we leave it
  * to you!
  */
class Optimizer(implicit config: Config) extends Phase[TacProg, TacProg]("optimizer", config) {

  /**
    * Transformer entry.
    *
    * @param input a TAC program
    * @return also a TAC program, but optimized (currently equals `input`)
    */
  override def transform(input: TacProg): TacProg = input

  /**
    * After generating the optimized TAC program, dump it and start the simulator if necessary.
    *
    * @param program Tac program
    */
  override def onSucceed(program: TacProg): Unit = {
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
