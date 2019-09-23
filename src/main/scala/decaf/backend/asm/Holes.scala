package decaf.backend.asm

import decaf.lowlevel.instr.{PseudoInstr, Temp}

/**
  * A trick to encode some holes as normal pseudo instructions.
  *
  * @see [[decaf.backend.asm.Holes.CallerSave]]
  * @see [[decaf.backend.asm.Holes.CallerRestore]]
  */
object Holes {

  object CallerSave extends PseudoInstr(new Array[Temp](0), new Array[Temp](0)) {
    override def toString: String = "# TODO: caller save"
  }

  object CallerRestore extends PseudoInstr(new Array[Temp](0), new Array[Temp](0)) {
    override def toString: String = "# TODO: caller restore"
  }

}
