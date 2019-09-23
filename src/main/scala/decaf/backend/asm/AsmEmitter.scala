package decaf.backend.asm

import decaf.lowlevel.AsmCodePrinter
import decaf.lowlevel.instr.{PseudoInstr, Reg}
import decaf.lowlevel.tac.{TacFunc, VTable}


/**
  * Emit assembly code.
  *
  * @param platformName    target platform name
  * @param allocatableRegs platform-specific registers accounted for register allocation, i.e. general registers
  * @param callerSaveRegs  platform-specific registers that need be saved by caller.
  */
abstract class AsmEmitter(val platformName: String, val allocatableRegs: Array[Reg], val callerSaveRegs: Array[Reg]) {
  /**
    * Emit assembly code for a virtual table.
    *
    * @param vtbl virtual table
    */
  def emitVTable(vtbl: VTable): Unit

  /**
    * Instruction selection for a TAC function.
    * <p>
    * Since no register allocation is done, the generated instructions may still contain pseudo registers (temps).
    *
    * @param func TAC function
    * @return a pair of the instruction sequence, and the basic info of the function
    */
  def selectInstr(func: TacFunc): (List[PseudoInstr], SubroutineInfo)

  /**
    * Call this when all virtual tables are done, and you want to emit code for subroutines.
    */
  def emitSubroutineBegin(): Unit

  /**
    * Begin to emit code for a subroutine.
    *
    * @param info basic info of this subroutine
    * @return emitter of this subroutine
    */
  def emitSubroutine(info: SubroutineInfo): SubroutineEmitter

  /**
    * Call this when all subroutines are done, and you want to finish.
    *
    * @return string representation of the emitted assembly code
    */
  def emitEnd(): String

  override def toString: String = platformName

  /**
    * Assembly code pretty printer.
    */
  final protected[asm] val printer = new AsmCodePrinter
}
