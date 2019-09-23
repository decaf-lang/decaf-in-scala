package decaf.backend.asm

import java.io.PrintWriter
import java.util.logging.Level

import decaf.backend.dataflow.{CFG, LivenessAnalyzer}
import decaf.backend.reg.RegAlloc
import decaf.driver.{Config, Phase}
import decaf.lowlevel.instr.PseudoInstr
import decaf.lowlevel.log.Log
import decaf.lowlevel.tac.TacProg
import decaf.printing.PrettyCFG
import decaf.util.Conversions._

/**
  * The assembly code generation phase: translate a TAC program to assembly code.
  *
  * @param emitter  helper assembly code emitter
  * @param regAlloc register allocator
  */
class Asm(val emitter: AsmEmitter, val regAlloc: RegAlloc) extends Phase[TacProg, String]("asm: " + emitter) {

  override def transform(prog: TacProg): String = {
    Log.info("phase: asm")
    val analyzer = new LivenessAnalyzer[PseudoInstr]

    for (vtbl <- prog.vtables) {
      Log.info("emit vtable for %s", vtbl.className)
      emitter.emitVTable(vtbl)
    }

    emitter.emitSubroutineBegin()
    for (func <- prog.funcs) {
      Log.info("emit func for %s", func.entry.prettyString)
      val (instrSeq, info) = emitter.selectInstr(func)
      val cfg = CFG.buildFrom(instrSeq)
      analyzer(cfg)
      Log.ifLoggable(Level.FINE, printer => new PrettyCFG[PseudoInstr](printer).pretty(cfg))
      regAlloc(cfg, info)
    }

    emitter.emitEnd()
  }

  override def post(code: String)(implicit config: Config): Unit = {
    if (config.target.equals(Config.Target.PA5)) {
      val path = config.dstDir / config.sourceBaseName + ".s"
      val printer = new PrintWriter(path)
      printer.print(code)
      printer.close()
    }
  }
}
