package decaf.backend.dataflow

import decaf.lowlevel.instr.{PseudoInstr, Temp}

import scala.collection.mutable

/**
  * Perform liveness analysis.
  *
  * @tparam I type of instructions
  */
class LivenessAnalyzer[I <: PseudoInstr] extends Function[CFG[I], Unit] {

  override def apply(graph: CFG[I]): Unit = {
    for (bb <- graph) {
      computeDefAndLiveUseFor(bb)
      bb.liveIn = new mutable.TreeSet[Temp]
      bb.liveIn.addAll(bb.liveUse)
      bb.liveOut = new mutable.TreeSet[Temp]
    }

    var changed: Boolean = true
    do {
      changed = false
      for (bb <- graph) {
        graph.getSucc(bb.id).foreach { next =>
          bb.liveOut.addAll(graph.getBlock(next).liveIn)
        }
        bb.`def`.foreach(bb.liveOut.remove)

        val old = bb.liveIn.size
        bb.liveIn.addAll(bb.liveOut)
        if (bb.liveIn.size > old) changed = true

        graph.getSucc(bb.id).foreach { next =>
          bb.liveOut.addAll(graph.getBlock(next).liveIn)
        }
      }
    } while (changed)

    graph.foreach(analyzeLivenessForEachLocIn)
  }

  /**
    * Compute the `def and `liveUse` set for basic block `bb`.
    *
    * Recall the definition:
    *
    *    - `def`: set of all variables (i.e. temps) that are assigned to a value. Thus, we simply union all the
    * written temps of every instruction.
    *    - `liveUse`: set of all variables (i.e. temps) that are used before they are assigned to a value in this
    * basic block. Note this is NOT simply equal to the union set all read temps, but only those are not yet
    * assigned/reassigned.
    *
    * @param bb basic block
    **/
  private def computeDefAndLiveUseFor(bb: BasicBlock[I]): Unit = {
    bb.`def` = new mutable.TreeSet[Temp]
    bb.liveUse = new mutable.TreeSet[Temp]
    for (loc <- bb) {
      loc.instr.getRead.forEach { t =>
        if (!bb.`def`.contains(t)) // used before being assigned to a value
        {
          bb.liveUse.add(t)
        }
      }
      loc.instr.getWritten.forEach(bb.`def`.add)
    }
  }

  /**
    * Perform liveness analysis for every single location in a basic block, so that we know at each program location,
    * which variables stay alive.
    *
    * Idea: realizing that every location loc can be regarded as a "mini" basic block -- a block containing that
    * instruction solely, then the data flow equations also hold, and the situation becomes much simpler:
    *
    *     - `loc.liveOut = loc.next.liveIn`
    *     - `loc.def` is simply the set of written temps
    *     - `loc.liveUse` is simply the set of read temps, since it is impossible to read and write a same temp
    * simultaneously
    * So you see, to back propagate every location solves the problem.
    *
    * @param bb the basic block
    */
  private def analyzeLivenessForEachLocIn(bb: BasicBlock[I]): Unit = {
    val liveOut = new mutable.TreeSet[Temp]
    liveOut.addAll(bb.liveOut)
    for (loc <- bb.backwardIterator) {
      loc.liveOut = new mutable.TreeSet[Temp]
      loc.liveOut.addAll(liveOut)
      // Order is important here, because in an instruction, one temp can be both read and written, e.g.
      // in `_T1 = _T1 + _T2`, `_T1` must be alive before execution.
      loc.instr.getWritten.forEach(liveOut.remove)
      loc.instr.getRead.forEach(liveOut.add)
      loc.liveIn = new mutable.TreeSet[Temp]
      loc.liveIn.addAll(liveOut)
    }
    // assert liveIn == bb.liveIn
  }
}
