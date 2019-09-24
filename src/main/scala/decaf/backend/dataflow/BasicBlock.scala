package decaf.backend.dataflow

import decaf.lowlevel.instr.{PseudoInstr, Temp}
import decaf.lowlevel.label.Label

import scala.collection.mutable

/**
  * A basic block in a control flow graph.
  *
  *
  * @tparam I type of the instructions stored in the block
  */
trait BasicBlock[I <: PseudoInstr] extends Iterable[Loc[I]] {

  /**
    * Id, MUST be unique in one control flow graph.
    */
  val id: Int

  /**
    * Label located at the entry of this block, if any.
    */
  val label: Option[Label]

  /**
    * Is this block labeled with a given `label`?
    *
    * @param label label
    * @return true if the block is labeled with the `label`
    */
  def isLabeledWith(label: Label): Boolean = this.label match {
    case Some(lbl) => lbl == label
    case None => false
  }

  /**
    * Iterator to traverse all locations in this block.
    */
  def iterator: Iterator[Loc[I]]

  /**
    * Iterator to traverse all sequential locations in this block.
    */
  def seqIterator: Iterator[Loc[I]]

  /**
    * Iterator to traverse all locations in this block in backward/reverse order in this block.
    */
  def backwardIterator: Iterator[Loc[I]] = reversed.iterator

  // For data flow analysis

  var `def`: mutable.Set[Temp] = _

  var liveUse: mutable.Set[Temp] = _

  var liveIn: mutable.Set[Temp] = _

  var liveOut: mutable.Set[Temp] = _
}

/**
  * A basic block which contains all sequential instructions.
  * After the execution of this block, the next block will be executed.
  *
  * @param id       id
  * @param label    label (if any)
  * @param instrSeq (sequential) instruction sequence
  * @tparam I type of instruction
  */
case class ContinuousBasicBlock[I <: PseudoInstr](id: Int, label: Option[Label], instrSeq: List[Loc[I]])
  extends BasicBlock[I] {

  override def iterator: Iterator[Loc[I]] = instrSeq.iterator

  override def seqIterator: Iterator[Loc[I]] = instrSeq.iterator
}

/**
  * A basic block which ends with a jump instruction.
  * After the execution of this block, the `jumpTo` block will be executed
  *
  * @param id         id
  * @param label      label (if any)
  * @param sequential sequential instruction sequence
  * @param jump       the jump instruction that ends the block, followed by sequential instructions
  * @tparam I type of instruction
  */
case class EndByJumpBasicBlock[I <: PseudoInstr](id: Int, label: Option[Label], sequential: List[Loc[I]],
                                                 jump: Loc[I]) extends BasicBlock[I] {

  override def iterator: Iterator[Loc[I]] = (sequential :+ jump).iterator

  override def seqIterator: Iterator[Loc[I]] = sequential.iterator

  def jumpTo: Label = jump.instr.label
}

/**
  * A basic block which ends with a conditional jump instruction.
  * After the execution of this block, either the `mayJumpTo` block will be executed (if condition holds), or the next
  * block will be executed.
  *
  * @param id         id
  * @param label      label (if any)
  * @param sequential sequential instruction sequence
  * @param jump       the jump instruction that ends the block, followed by sequential instructions
  * @tparam I type of instruction
  */
case class EndByCondJumpBasicBlock[I <: PseudoInstr](id: Int, label: Option[Label], sequential: List[Loc[I]],
                                                     jump: Loc[I]) extends BasicBlock[I] {

  override def iterator: Iterator[Loc[I]] = (sequential :+ jump).iterator

  override def seqIterator: Iterator[Loc[I]] = sequential.iterator

  def mayJumpTo: Label = jump.instr.label
}

/**
  * A basic block which ends with a return instruction.
  * After the execution of this block, the function/subroutine returns.
  *
  * @param id         id
  * @param label      label (if any)
  * @param sequential sequential instruction sequence
  * @param ret        the return instruction that ends the block, followed by sequential instructions
  * @tparam I type of instruction
  */
case class EndByReturnBasicBlock[I <: PseudoInstr](id: Int, label: Option[Label], sequential: List[Loc[I]],
                                                   ret: Loc[I]) extends BasicBlock[I] {

  override def iterator: Iterator[Loc[I]] = (sequential :+ ret).iterator

  override def seqIterator: Iterator[Loc[I]] = sequential.iterator
}
