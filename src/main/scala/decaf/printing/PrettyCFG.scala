package decaf.printing

import decaf.backend.dataflow.{BasicBlock, CFG}
import decaf.lowlevel.instr.PseudoInstr
import decaf.lowlevel.log.IndentPrinter

class PrettyCFG[I <: PseudoInstr](printer: IndentPrinter) extends PrettyPrinter[CFG[I]](printer) {
  override def pretty(graph: CFG[I]): Unit = {
    printer.println("CFG")
    indent {
      graph.foreach(prettyBasicBlock)
    }
    printer.println()
  }

  def prettyBasicBlock(bb: BasicBlock[I]): Unit = {
    printer.format("BLOCK %d", bb.id)
    bb.label.foreach { l => printer.format(" (%s)", l.prettyString) }
    printer.println()

    indent {
      printer.prettyFormatLn("def     = %s", bb.`def`)
      printer.prettyFormatLn("liveUse = %s", bb.liveUse)
      printer.prettyFormatLn("liveIn  = %s", bb.liveIn)
      printer.prettyFormatLn("liveOut = %s", bb.liveOut)
      printer.println()

      if (bb.isEmpty) printer.println("<empty>")
      else bb.foreach { loc =>
        printer.prettyFormatLn("%s # liveOut = %s", loc.instr, loc.liveOut)
      }
    }
  }
}
