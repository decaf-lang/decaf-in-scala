package decaf.backend.dataflow

import decaf.lowlevel.instr.PseudoInstr
import decaf.lowlevel.label.Label

import scala.collection.mutable

/**
  * Control flow graph.
  *
  * In a control flow graph, nodes are basic blocks, and an edge `(i, j)` indicates that basic block `j` is a reachable
  * successor of basic block `i`, i.e. after the execution of `i`, it is possible to execute `j`.
  *
  * REQUIRE: basic block ids must be in range `0..nodes.length - 1`.
  *
  * @param nodes nodes
  * @param edges edges
  * @tparam I type of the instruction stored in the block
  */
class CFG[I <: PseudoInstr](nodes: List[BasicBlock[I]], edges: List[(Int, Int)])
  extends Iterable[BasicBlock[I]] {

  /**
    * Get basic block by id.
    *
    * @param id basic block id
    * @return basic block
    */
  def getBlock(id: Int): BasicBlock[I] = blocks(id)

  /**
    * Get predecessors.
    *
    * @param id basic block id
    * @return its predecessors
    */
  def getPrev(id: Int): Set[Int] = prevs(id).toSet

  /**
    * Get successors.
    *
    * @param id basic block id
    * @return its successors
    */
  def getSucc(id: Int): Set[Int] = succs(id).toSet

  /**
    * Get in-degree.
    *
    * @param id basic block id
    * @return its in-degree
    */
  def getInDegree(id: Int): Int = prevs(id).size

  /**
    * Get out-degree.
    *
    * @param id basic block id
    * @return its out-degree
    */
  def getOutDegree(id: Int): Int = succs(id).size

  /**
    * Iterator to traverse all basic blocks, in order.
    */
  override def iterator: Iterator[BasicBlock[I]] = blocks.iterator

  private val blocks = new Array[BasicBlock[I]](nodes.length)

  nodes.foreach { bb =>
    blocks(bb.id) = bb
  }

  private val prevs = new Array[mutable.Set[Int]](nodes.length)

  for (i <- prevs.indices) {
    prevs(i) = new mutable.TreeSet[Int]
  }

  private val succs = new Array[mutable.Set[Int]](nodes.length)

  for (i <- succs.indices) {
    succs(i) = new mutable.TreeSet[Int]
  }

  edges.foreach {
    case (u, v) =>
      succs(u) += v
      prevs(v) += u
  }
}

object CFG {

  /**
    * Build a control flow graph from a sequence of instructions (in one function/subroutine).
    *
    * @param seq instruction sequence
    * @return corresponding control flow graph
    */
  def buildFrom[I <: PseudoInstr](seq: List[I]): CFG[I] = {

    @scala.annotation.tailrec
    def build(seq: List[I], label: Option[Label], bbs: List[BasicBlock[I]]): List[BasicBlock[I]] = {
      if (seq.isEmpty) {
        bbs
      } else {
        val (sequential, remainder) = seq.span(_.isSequential)
        assert(remainder.nonEmpty)
        val end :: rest = remainder
        end.kind match {
          case PseudoInstr.Kind.LABEL =>
            val bb = ContinuousBasicBlock(bbs.length, label, sequential.map(Loc[I]))
            build(rest, Some(end.label), bb :: bbs)
          case PseudoInstr.Kind.JMP =>
            val bb = EndByJumpBasicBlock(bbs.length, label, sequential.map(Loc[I]), Loc(end))
            build(rest, None, bb :: bbs)
          case PseudoInstr.Kind.COND_JMP =>
            val bb = EndByCondJumpBasicBlock(bbs.length, label, sequential.map(Loc[I]), Loc(end))
            build(rest, None, bb :: bbs)
          case PseudoInstr.Kind.RET =>
            val bb = EndByReturnBasicBlock(bbs.length, label, sequential.map(Loc[I]), Loc(end))
            build(rest, None, bb :: bbs)
        }
      }
    }

    assert(seq.nonEmpty)
    val nodes = build(if (seq.head.isLabel && seq.head.label.isFunc) seq.tail else seq, None, Nil)
    val edges = nodes.flatMap {
      case bb: ContinuousBasicBlock[I] => List((bb.id, bb.id + 1))
      case bb: EndByJumpBasicBlock[I] => List((bb.id, nodes.find(_.isLabeledWith(bb.jumpTo)).get.id))
      case bb: EndByCondJumpBasicBlock[I] =>
        List((bb.id, bb.id + 1), (bb.id, nodes.find(_.isLabeledWith(bb.mayJumpTo)).get.id))
      case _: EndByReturnBasicBlock[I] => Nil
    }

    new CFG[I](nodes, edges)
  }
}