package decaf.backend.asm

import decaf.lowlevel.label.FuncLabel

/**
  * Basic info of subroutine.
  *
  * @param funcLabel label of the function entry
  * @param numArgs   number of arguments
  * @param hasCalls  does this subroutine call others?
  * @param argsSize  max. stack size needed to store arguments
  */
case class SubroutineInfo(funcLabel: FuncLabel, numArgs: Int, hasCalls: Boolean, argsSize: Int)
