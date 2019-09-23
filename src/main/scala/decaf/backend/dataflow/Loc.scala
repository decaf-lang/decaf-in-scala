package decaf.backend.dataflow

import decaf.lowlevel.instr.{PseudoInstr, Temp}

import scala.collection.mutable

/**
  * A program location in a basic block, i.e. instruction with results of liveness analysis.
  */
case class Loc[I <: PseudoInstr](instr: I) {
  var liveIn: mutable.Set[Temp] = _
  var liveOut: mutable.Set[Temp] = _
}