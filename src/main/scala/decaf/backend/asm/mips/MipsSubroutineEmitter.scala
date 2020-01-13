package decaf.backend.asm.mips

import decaf.backend.asm.{SubroutineEmitter, SubroutineInfo}
import decaf.lowlevel.Mips
import decaf.lowlevel.instr.{NativeInstr, Reg, Temp}
import decaf.lowlevel.tac.TacInstr
import decaf.lowlevel.label.Label

import scala.collection.mutable

/**
  * Emit MIPS assembly code for a subroutine.
  *
  * Recall the stack frame of a MIPS subroutine looks this:
  * {{{
  * previous stack frame ...
  * SP + 4n + 40 + : local data m - 1
  * 4(m - 1)
  * ...
  * SP + 4n + 40   : local data 0
  * SP + 4n + 36   : ($RA)
  * SP + 4n + 32   : ($S8)
  * ...
  * SP + 4n + 0    : ($S0)
  * SP + 4(n - 1)  : arg n - 1
  * ...
  * SP + 16        : arg 4
  * ...
  * SP             : (arg 0)
  * }}}
  * The parenthesized slots may not be used, but to make our life easier, we always reserve them.
  */
class MipsSubroutineEmitter private[mips](emitter: MipsAsmEmitter, info: SubroutineInfo)
  extends SubroutineEmitter(emitter, info) {

  override def emitStoreToStack(src: Reg, temp: Temp): Unit = {
    var isArgOrLocal = true
    if (temp.index < info.numArgs) { // Always map arg `i` to `SP + 4 * i`.
        if (!offsets.contains(temp)) {
            offsets(temp) = 4 * temp.index
        }
        isArgOrLocal = true
    }
    else {
        if (!offsets.contains(temp)) {
            offsets(temp) = nextLocalOffset
            nextLocalOffset += 4
        }
        isArgOrLocal = false
    }
    buf += new Mips.NativeStoreWord(src, Mips.SP, offsets(temp), isArgOrLocal)
  }

  override def emitLoadFromStack(dst: Reg, src: Temp): Unit = {
    var isArgOrLocal = true
    if (src.index < info.numArgs) { // arg
        if (!offsets.contains(src)) {
            val offset = 4 * src.index
            offsets.put(src, offset)
            buf += new Mips.NativeLoadWord(dst, Mips.SP, offset, true)
            return
        }
        isArgOrLocal = true
    }
    else {
        if (!offsets.contains(src)) {
            throw new IllegalArgumentException("offsets doesn't contain " + src + " when loading " + dst)
        }
        isArgOrLocal = false
    }
    buf += new Mips.NativeLoadWord(dst, Mips.SP, offsets(src), isArgOrLocal)
  }

  override def emitCall(argsNum: Int): Unit = {
    maxArgsNum = maxArgsNum max argsNum
  }

  override def emitMove(dst: Reg, src: Reg): Unit = {
    buf += new Mips.NativeMove(dst, src)
  }

  override def emitNative(instr: NativeInstr): Unit = {
    buf += instr
  }

  override def emitLabel(label: Label): Unit = {
    buf += new Mips.MipsLabel(label).toNative(new Array[Reg](0), new Array[Reg](0))
  }

  override def emitEnd(used: Set[Reg]): Unit = {
    maxArgsSize = maxArgsNum * 4;
    frameLength = nextLocalOffset + maxArgsSize

    printer.printComment("start of prologue")
    printer.printInstr(new Mips.SPAdd(-frameLength), "push stack frame")
    if (info.hasCalls) {
      // 40 whitespaces for 10 saved reg ($s0 ~ $s8),
      // where in common mips implementation there's only 9 regs.
      printer.printInstr(new Mips.NativeStoreWord(Mips.RA, Mips.SP, maxArgsSize + 40), "save the return address")
    }
    for {
      (reg, i) <- Mips.calleeSaved.zipWithIndex
      if used(reg)
    } {
      printer.printInstr(new Mips.NativeStoreWord(reg, Mips.SP, maxArgsSize + 4 * i), "save value of " + reg)
    }
    printer.printComment("end of prologue")
    printer.println()

    printer.printComment("start of body")
    for (i <- 0 until Math.min(info.numArgs, 4)) {
      printer.printInstr(new Mips.NativeStoreWord(Mips.argRegs(i), Mips.SP, frameLength + 4 * i), "save arg " + i)
    }
    buf.foreach(
        instr => {
            instr match {
                case nlw: Mips.NativeLoadWord => nlw.backfill(maxArgsNum, frameLength);
                case nsw: Mips.NativeStoreWord => nsw.backfill(maxArgsNum, frameLength);
                case _ =>
            }
            printer.printInstr(instr)
        }
    )
    printer.printComment("end of body")
    printer.println()

    printer.printLabel(new Label(info.funcLabel.name + Mips.EPILOGUE_SUFFIX))
    printer.printComment("start of epilogue")
    for {
      (reg, i) <- Mips.calleeSaved.zipWithIndex
      if used(reg)
    } {
      printer.printInstr(new Mips.NativeLoadWord(Mips.calleeSaved(i), Mips.SP, maxArgsSize + 4 * i),
        "restore value of $S" + i)
    }
    if (info.hasCalls) {
      printer.printInstr(new Mips.NativeLoadWord(Mips.RA, Mips.SP, maxArgsSize + 40), "restore the return address")
    }
    printer.printInstr(new Mips.SPAdd(frameLength), "pop stack frame")
    printer.printComment("end of epilogue")
    printer.println()

    printer.printInstr(new Mips.NativeReturn, "return")
    printer.println()
  }

  private val buf = new mutable.ArrayBuffer[NativeInstr]
  private var maxArgsNum = 4;
  private var maxArgsSize = 4 * 4;
  private var nextLocalOffset = 44
  private var frameLength = 0

  // The offset of parameters are negative,
  // as the length of the frame is not known at the native instructions are emitted.
  private val offsets = new mutable.TreeMap[Temp, Integer]

  printer.printLabel(info.funcLabel, "function " + info.funcLabel.prettyString)
}
