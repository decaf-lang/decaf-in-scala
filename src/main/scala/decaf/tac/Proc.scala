package decaf.tac

import Tac.InstrSeq

/**
  * Procedures/Functions. In TAC, a procedure consists of:
  * - a label of the entry point, so that our call instruction can jump into it and execute from the first instruction
  * - a parameter memo, recording which temps are reserved for passing actual parameters
  * - a sequence of instructions to be executed
  *
  * @param label     entry label
  * @param paramMemo parameter memo
  * @param code      sequence of instructions
  */
class Proc(val label: Label, val paramMemo: Tac.Memo, val code: InstrSeq)