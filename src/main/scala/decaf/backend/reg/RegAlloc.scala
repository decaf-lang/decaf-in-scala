package decaf.backend.reg

import decaf.backend.asm.{AsmEmitter, SubroutineInfo}
import decaf.backend.dataflow.CFG
import decaf.lowlevel.instr.PseudoInstr

/**
  * Register allocation.
  *
  * @param emitter assembly emitter
  */
abstract class RegAlloc(var emitter: AsmEmitter) {

  /**
    * Entry of the main algorithm.
    *
    * @param graph control flow graph
    * @param info  basic info of the associated subroutine
    */
  def apply(graph: CFG[PseudoInstr], info: SubroutineInfo): Unit
}
