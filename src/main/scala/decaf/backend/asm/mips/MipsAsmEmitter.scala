package decaf.backend.asm.mips

import decaf.backend.asm.{AsmEmitter, Holes, SubroutineInfo}
import decaf.lowlevel.Mips.STR_PREFIX
import decaf.lowlevel.instr.PseudoInstr
import decaf.lowlevel.label.{IntrinsicLabel, Label}
import decaf.lowlevel.tac._
import decaf.lowlevel.{Mips, StringUtils}
import decaf.util.Conversions._

import scala.collection.mutable

/**
  * Emit MIPS assembly code.
  */
final class MipsAsmEmitter extends AsmEmitter("mips", Mips.allocatableRegs, Mips.callerSaved) {

  override def emitVTable(vtbl: VTable): Unit = {
    printer.println(".data")
    printer.println(".align 2")
    printer.printLabel(vtbl.label, "virtual table for " + vtbl.className)
    if (vtbl.parent.isPresent) {
      val parent = vtbl.parent.get
      printer.println(".word %s    # parent: %s", parent.label, parent.className)
    }
    else {
      printer.println(".word 0    # parent: none")
    }
    val index = pool.add(vtbl.className)
    printer.println(".word %s%d    # class name", STR_PREFIX, index)
    vtbl.getItems.forEach { entry =>
      printer.println(".word %s    # member method", entry.name)
    }
    printer.println()
  }

  override def selectInstr(func: TacFunc): (List[PseudoInstr], SubroutineInfo) = {
    val selector = new MipsInstrSelector(func.entry)
    func.getInstrSeq.forEach(_.accept(selector))
    val info = new SubroutineInfo(func.entry, func.numArgs, selector.hasCall,
      Math.max(func.numArgs, selector.maxArgs) * 4)
    (selector.seq.toList, info)
  }

  override def emitSubroutineBegin(): Unit = {
    printer.println(".text")
  }

  override def emitSubroutine(info: SubroutineInfo) = new MipsSubroutineEmitter(this, info)

  override def emitEnd(): String = {
    if (usedIntrinsics.nonEmpty) {
      printer.println("# start of intrinsics")
      if (usedIntrinsics.contains(Intrinsic.READ_LINE.entry)) loadReadLine()
      if (usedIntrinsics.contains(Intrinsic.STRING_EQUAL.entry)) loadStringEqual()
      if (usedIntrinsics.contains(Intrinsic.PRINT_BOOL.entry)) loadPrintBool()
      printer.println("# end of intrinsics")
      printer.println()
    }
    printer.println("# start of constant strings")
    printer.println(".data")
    var i = 0
    pool.forEach { str =>
      printer.printLabel(new Label(STR_PREFIX + i))
      printer.println(".asciiz %s", StringUtils.quote(str))
      i += 1
    }
    printer.println("# end of constant strings")
    printer.close
  }

  printer.println("# start of header")
  printer.println(".text")
  printer.println(".globl main")
  printer.println("# end of header")
  printer.println()

  private def loadReadLine(): Unit = {
    val loop = new Label(Intrinsic.READ_LINE.entry + "_loop")
    val exit = new Label(Intrinsic.READ_LINE.entry + "_exit")
    printer.printLabel(Intrinsic.READ_LINE.entry, "intrinsic: read line")
    printer.println("sw $a0, -4($sp)")
    printer.println("sw $a1, -8($sp)")
    printer.println("li $a0, 64    # allocate space, fixed size 64")
    printer.println("li $v0, 9     # memory allocation")
    printer.println("syscall")
    printer.println("move $a0, $v0")
    printer.println("li $a1, 64")
    printer.println("li $v0, 8     # read string")
    printer.println("syscall")
    printer.println("move $v0, $a0")
    printer.printLabel(loop)
    printer.println("lb $a1, ($a0)")
    printer.println("beqz $a1, %s", exit)
    printer.println("addi $a1, $a1, -10  # subtract ASCII newline")
    printer.println("beqz $a1, %s", exit)
    printer.println("addi $a0, $a0, 1")
    printer.println("j %s", loop)
    printer.printLabel(exit)
    printer.println("sb $a1, ($a0)")
    printer.println("lw $a0, -4($sp)")
    printer.println("lw $a1, -8($sp)")
    printer.println("jr $ra")
    printer.println()
  }

  private def loadStringEqual(): Unit = {
    val loop = new Label(Intrinsic.STRING_EQUAL.entry + "_loop")
    val exit = new Label(Intrinsic.STRING_EQUAL.entry + "_exit")
    printer.printLabel(Intrinsic.STRING_EQUAL.entry, "intrinsic: string equal")
    printer.println("sw $a2, -4($sp)")
    printer.println("sw $a3, -8($sp)")
    printer.println("li $v0, 1")
    printer.printLabel(loop)
    printer.println("lb $a2, ($a0)")
    printer.println("lb $a3, ($a1)")
    printer.println("seq $v0, $a2, $a3")
    printer.println("beqz $v0, %s", exit)
    printer.println("beqz $a2, %s", exit)
    printer.println("addiu $a0, $a0, 1")
    printer.println("addiu $a1, $a1, 1")
    printer.println("j %s", loop)
    printer.printLabel(exit)
    printer.println("lw $a2, -4($sp)")
    printer.println("lw $a3, -8($sp)")
    printer.println("jr $ra")
    printer.println()
  }

  private def loadPrintBool(): Unit = {
    val trueString = new Label(Intrinsic.PRINT_BOOL.entry + "_S_true")
    val falseString = new Label(Intrinsic.PRINT_BOOL.entry + "_S_false")
    val isFalse = new Label(Intrinsic.PRINT_BOOL.entry + "_false")
    printer.printLabel(Intrinsic.PRINT_BOOL.entry, "intrinsic: print bool")
    printer.println(".data")
    printer.printLabel(trueString)
    printer.println(".asciiz \"true\"")
    printer.printLabel(falseString)
    printer.println(".asciiz \"false\"")
    printer.println(".text")
    printer.println("li $v0, 4    # print string")
    printer.println("beqz $a0, %s", isFalse)
    printer.println("la $a0, %s", trueString)
    printer.println("syscall")
    printer.println("jr $ra")
    printer.printLabel(isFalse)
    printer.println("la $a0, %s", falseString)
    printer.println("syscall")
    printer.println("jr $ra")
  }

  private class MipsInstrSelector private[mips](var entry: Label) extends TacInstr.Visitor {

    private[mips] val seq = new mutable.ArrayBuffer[PseudoInstr]
    private[mips] var maxArgs = 0
    private var argCount = 0
    private[mips] var hasCall = false

    override def visitAssign(instr: TacInstr.Assign): Unit = {
      seq += new Mips.Move(instr.dst, instr.src)
    }

    override def visitLoadVTbl(instr: TacInstr.LoadVTbl): Unit = {
      seq += new Mips.LoadAddr(instr.dst, instr.vtbl.label)
    }

    override def visitLoadImm4(instr: TacInstr.LoadImm4): Unit = {
      seq += new Mips.LoadImm(instr.dst, instr.value)
    }

    override def visitLoadStrConst(instr: TacInstr.LoadStrConst): Unit = {
      val index = pool.add(instr.value)
      seq += new Mips.LoadAddr(instr.dst, new Label(STR_PREFIX + index))
    }

    override def visitUnary(instr: TacInstr.Unary): Unit = {
      val op = instr.op match {
        case TacInstr.Unary.Op.NEG => Mips.UnaryOp.NEG
        case TacInstr.Unary.Op.LNOT => Mips.UnaryOp.NOT
      }
      seq += new Mips.Unary(op, instr.dst, instr.operand)
    }

    override def visitBinary(instr: TacInstr.Binary): Unit = {
      val op = instr.op match {
        case TacInstr.Binary.Op.ADD => Mips.BinaryOp.ADD
        case TacInstr.Binary.Op.SUB => Mips.BinaryOp.SUB
        case TacInstr.Binary.Op.MUL => Mips.BinaryOp.MUL
        case TacInstr.Binary.Op.DIV => Mips.BinaryOp.DIV
        case TacInstr.Binary.Op.MOD => Mips.BinaryOp.REM
        case TacInstr.Binary.Op.EQU => Mips.BinaryOp.SEQ
        case TacInstr.Binary.Op.NEQ => Mips.BinaryOp.SNE
        case TacInstr.Binary.Op.LES => Mips.BinaryOp.SLT
        case TacInstr.Binary.Op.LEQ => Mips.BinaryOp.SLE
        case TacInstr.Binary.Op.GTR => Mips.BinaryOp.SGT
        case TacInstr.Binary.Op.GEQ => Mips.BinaryOp.SGE
        case TacInstr.Binary.Op.LAND => Mips.BinaryOp.AND
        case TacInstr.Binary.Op.LOR => Mips.BinaryOp.OR
      }
      seq += new Mips.Binary(op, instr.dst, instr.lhs, instr.rhs)
    }

    override def visitBranch(instr: TacInstr.Branch): Unit = {
      seq += new Mips.Jump(instr.target)
    }

    override def visitCondBranch(instr: TacInstr.CondBranch): Unit = {
      val op = instr.op match {
        case TacInstr.CondBranch.Op.BEQZ => Mips.BranchOp.BEQZ
        case TacInstr.CondBranch.Op.BNEZ => Mips.BranchOp.BNEZ
      }
      seq += new Mips.Branch(op, instr.cond, instr.target)
    }

    override def visitReturn(instr: TacInstr.Return): Unit = {
      instr.value.foreach { seq += new Mips.Move(Mips.V0, _) }
      seq += new Mips.JumpToEpilogue(entry)
    }

    override def visitParm(instr: TacInstr.Parm): Unit = {
      if (argCount < 4) {
        seq += new Mips.Move(Mips.argRegs(argCount), instr.value)
      } else {
        seq += new Mips.StoreWord(instr.value, Mips.SP, argCount * 4)
      }
      argCount += 1
    }

    override def visitIndirectCall(instr: TacInstr.IndirectCall): Unit = {
      hasCall = true
      callerSave()
      seq += new Mips.JumpAndLinkReg(instr.entry)
      callerRestore()
      argCount = 0
      instr.dst.foreach { seq += new Mips.Move(_, Mips.V0) }
    }

    override def visitDirectCall(instr: TacInstr.DirectCall): Unit = {
      hasCall = true
      if (instr.entry.isIntrinsic) {
        // special case: inline or embed the code (no registers need be saved)
        val il = instr.entry.asInstanceOf[IntrinsicLabel]
        il.opcode match {
          case Intrinsic.Opcode.ALLOCATE =>
            seq += new Mips.LoadImm(Mips.V0, 9) // memory allocation
            seq += new Mips.Syscall
          case Intrinsic.Opcode.READ_INT =>
            seq += new Mips.LoadImm(Mips.V0, 5) // read integer
            seq += new Mips.Syscall
          case Intrinsic.Opcode.PRINT_INT =>
            seq += new Mips.LoadImm(Mips.V0, 1) // print integer
            seq += new Mips.Syscall
          case Intrinsic.Opcode.PRINT_STRING =>
            seq += new Mips.LoadImm(Mips.V0, 4) // print string
            seq += new Mips.Syscall
          case Intrinsic.Opcode.HALT =>
            seq += new Mips.LoadImm(Mips.V0, 10) // exit
            seq += new Mips.Syscall
          case other =>
            seq += new Mips.JumpAndLink(il)
            usedIntrinsics += il
        }
      } else { // normal call
        callerSave()
        seq += new Mips.JumpAndLink(new Label(instr.entry.name))
        callerRestore()
      }

      argCount = 0
      // finally
      instr.dst.foreach { seq += new Mips.Move(_, Mips.V0) }
    }

    private def callerSave(): Unit = {
      maxArgs = Math.max(maxArgs, argCount)
      seq += Holes.CallerSave
    }

    private def callerRestore(): Unit = {
      seq += Holes.CallerRestore
    }

    override def visitMemory(instr: TacInstr.Memory): Unit = {
      instr.op match {
        case TacInstr.Memory.Op.LOAD =>
          seq += new Mips.LoadWord(instr.dst, instr.base, instr.offset)
        case TacInstr.Memory.Op.STORE =>
          seq += new Mips.StoreWord(instr.dst, instr.base, instr.offset)
      }
    }

    override def visitMark(instr: TacInstr.Mark): Unit = {
      seq += new Mips.MipsLabel(instr.label)
    }
  }

  private val pool = new StringPool
  private val usedIntrinsics = new mutable.TreeSet[IntrinsicLabel]
}
