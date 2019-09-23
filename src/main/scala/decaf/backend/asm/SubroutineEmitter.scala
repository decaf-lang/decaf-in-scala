package decaf.backend.asm

import decaf.lowlevel.AsmCodePrinter
import decaf.lowlevel.instr.{NativeInstr, Reg, Temp}
import decaf.lowlevel.label.Label

/**
  * Emit assembly code for a subroutine.
  *
  * @param emitter attached [[AsmEmitter]]
  * @param info    basic info of this subroutine
  */
abstract class SubroutineEmitter protected(val emitter: AsmEmitter, var info: SubroutineInfo) {
  /**
    * Append an assembly instruction that stores the value of a register to stack.
    *
    * @param src  source register
    * @param temp the bound temp of `src`
    */
  def emitStoreToStack(src: Reg, temp: Temp): Unit

  /**
    * Append an assembly instruction that loads a value from stack to a register.
    *
    * @param dst destination register
    * @param src source temp
    */
  def emitLoadFromStack(dst: Reg, src: Temp): Unit

  /**
    * Append an assembly instruction that copies value between two registers.
    *
    * @param dst destination register
    * @param src source register
    */
  def emitMove(dst: Reg, src: Reg): Unit

  /**
    * Append a given assembly instruction.
    *
    * @param instr assembly instruction
    */
  def emitNative(instr: NativeInstr): Unit

  /**
    * Append a label.
    *
    * @param label label
    */
  def emitLabel(label: Label): Unit

  /**
    * Call this when you have appended all user and synthetic (by register allocation algorithm) instructions of
    * this subroutine.
    *
    * @param used used registers during register allocation
    */
  def emitEnd(used: Set[Reg]): Unit

  /**
    * Assembly code pretty printer.
    */
  protected var printer: AsmCodePrinter = emitter.printer
}