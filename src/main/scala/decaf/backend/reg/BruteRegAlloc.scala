package decaf.backend.reg

import decaf.backend.asm.{AsmEmitter, Holes, SubroutineEmitter, SubroutineInfo}
import decaf.backend.dataflow._
import decaf.lowlevel.instr.{PseudoInstr, Reg, Temp}

import scala.collection.mutable
import scala.util.Random

/**
  * Brute force greedy register allocation algorithm.
  * <p>
  * To make our life easier, don't consider any special registers that may be used during call.
  */
final class BruteRegAlloc(emitter: AsmEmitter) extends RegAlloc(emitter) {

  override def apply(graph: CFG[PseudoInstr], info: SubroutineInfo): Unit = {
    val subEmitter = emitter.emitSubroutine(info)
    implicit val ctx = new Context(subEmitter)
    for (bb <- graph) {
      bb.label.foreach(subEmitter.emitLabel)
      localAlloc(bb)
    }
    subEmitter.emitEnd(ctx.used.toSet)
  }

  private class Context(val subEmitter: SubroutineEmitter) {

    val regOf = new mutable.TreeMap[Temp, Reg]
    val tempOf = new mutable.TreeMap[Reg, Temp]
    val occupied = new mutable.TreeSet[Reg]
    val used = new mutable.TreeSet[Reg]
  }

  private def bind(temp: Temp, reg: Reg)(implicit ctx: Context): Unit = {
    ctx.used += reg
    ctx.occupied += reg

    ctx.regOf(temp) = reg
    ctx.tempOf(reg) = temp
  }

  private def unbind(temp: Temp)(implicit ctx: Context): Unit = {
    if (ctx.regOf.contains(temp)) {
      ctx.occupied.remove(ctx.regOf(temp))
      ctx.regOf.remove(temp)
    }
  }

  /**
    * Main algorithm of local register allocation à la brute-force. Basic idea:
    * <ul>
    * <li>Allocation is preformed block-by-block.</li>
    * <li>Assume that every allocatable register is unoccupied before entering every basic block.</li>
    * <li>For every read (src) and written (dst) temp {@code t} in every pseudo instruction, attempt the following
    * in order:</li>
    * <li><ol>
    * <li>{@code t} is already bound to a register: keep on using it.</li>
    * <li>If there exists an available (unoccupied, or the occupied temp is no longer alive) register,
    * then bind to it.</li>
    * <li>Arbitrarily pick a general register, spill its value to stack, and then bind to it.</li>
    * </ol></li>
    * </ul>
    * <p>
    * The output assembly code is maintained by {@code emitter}.
    *
    * @param bb  the basic block which the algorithm performs on
    * @param ctx context
    * @see #allocRegFor
    */
  private def localAlloc(bb: BasicBlock[PseudoInstr])(implicit ctx: Context): Unit = {
    ctx.regOf.clear()
    ctx.tempOf.clear()
    ctx.occupied.clear()

    val callerNeedSave = new mutable.ArrayBuffer[Reg]

    bb.seqIterator.foreach {
      // Handle special instructions on caller save/restore.
      case loc if loc.instr.isInstanceOf[Holes.CallerSave.type] =>
        for {
          reg <- emitter.callerSaveRegs
          if ctx.occupied(reg) && loc.liveOut(ctx.tempOf(reg))
        } yield {
          callerNeedSave += reg
          ctx.subEmitter.emitStoreToStack(reg, ctx.tempOf(reg))
        }
      case loc if loc.instr.isInstanceOf[Holes.CallerRestore.type] =>
        callerNeedSave.foreach { reg =>
          ctx.subEmitter.emitLoadFromStack(reg, ctx.tempOf(reg))
        }
        callerNeedSave.clear()

      // For normal instructions: allocate registers for every read/written temp. Skip the already specified
      // special registers.
      case loc => allocForLoc(loc)
    }

    // Before we leave a basic block, we must copy values of all live variables from registers (if exist)
    // to stack, as all these registers will be reset (as unoccupied) when entering another basic block.
    for {
      temp <- bb.liveOut
      if ctx.regOf.contains(temp)
    } {
      ctx.subEmitter.emitStoreToStack(ctx.regOf(temp), temp)
    } // Handle the last instruction, if it is a branch/return block.
    bb match {
      case _: ContinuousBasicBlock[PseudoInstr] => // skip
      case bb: EndByJumpBasicBlock[PseudoInstr] => allocForLoc(bb.jump)
      case bb: EndByCondJumpBasicBlock[PseudoInstr] => allocForLoc(bb.jump)
      case bb: EndByReturnBasicBlock[PseudoInstr] => allocForLoc(bb.ret)
    }
  }

  private def allocForLoc(loc: Loc[PseudoInstr])(implicit ctx: Context): Unit = {
    val instr = loc.instr
    val srcRegs = new Array[Reg](instr.srcs.length)
    val dstRegs = new Array[Reg](instr.dsts.length)

    for (i <- instr.srcs.indices) {
      instr.srcs(i) match {
        case reg: Reg => srcRegs(i) = reg
        case temp => srcRegs(i) = allocRegFor(temp, true, loc.liveIn)
      }
    }

    for (i <- instr.dsts.indices) {
      instr.dsts(i) match {
        case reg: Reg => dstRegs(i) = reg
        case temp => dstRegs(i) = allocRegFor(temp, false, loc.liveIn)
      }
    }

    ctx.subEmitter.emitNative(instr.toNative(dstRegs, srcRegs))
  }

  /**
    * Allocate a register for a temp.
    *
    * @param temp   temp appeared in the pseudo instruction
    * @param isRead true = read, false = write
    * @param live   set of live temps before executing this instruction
    * @param ctx    context
    * @return register for use
    */
  private def allocRegFor(temp: Temp, isRead: Boolean, live: mutable.Set[Temp])(implicit ctx: Context): Reg = {
    // Best case: the value of `temp` is already in register.
    if (ctx.regOf.contains(temp)) return ctx.regOf(temp)

    // First attempt: find an unoccupied register, or one whose value is no longer alive at this location.
    for (reg <- emitter.allocatableRegs) {
      if (!ctx.occupied(reg) || !live.contains(ctx.tempOf(reg))) {
        if (isRead) { // Since `reg` is uninitialized, we must load the latest value of `temp`, from stack, to `reg`.
          ctx.subEmitter.emitLoadFromStack(reg, temp)
        }
        if (ctx.occupied(reg)) unbind(ctx.tempOf(reg))
        bind(temp, reg)
        return reg
      }
    }

    // Last attempt: all registers are occupied, so we have to spill one of them.
    // To avoid the situation where the first register is consecutively spilled, a reasonable heuristic
    // is to randomize our choice among all of them.
    val reg = emitter.allocatableRegs(random.nextInt(emitter.allocatableRegs.length))
    ctx.subEmitter.emitStoreToStack(reg, ctx.tempOf(reg))
    unbind(ctx.tempOf(reg))
    bind(temp, reg)
    if (isRead) {
      ctx.subEmitter.emitLoadFromStack(reg, temp)
    }
    reg
  }

  /**
    * Random number generator.
    */
  private val random = new Random
}
