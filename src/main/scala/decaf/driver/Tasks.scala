package decaf.driver

import java.io.InputStream

import decaf.backend.asm.Asm
import decaf.backend.asm.mips.MipsAsmEmitter
import decaf.backend.opt.Optimizer
import decaf.backend.reg.BruteRegAlloc
import decaf.frontend.parsing.Parser
import decaf.frontend.tac.TacGen
import decaf.frontend.tree.TypedTree
import decaf.frontend.typecheck.{Namer, Typer}
import decaf.jvm.{JVMClass, JVMGen}
import decaf.lowlevel.tac.TacProg

/**
  * Supported tasks of Decaf compiler.
  */
class Tasks(implicit opt: Config) {

  val parse = new Parser

  val typeCheck: Task[InputStream, TypedTree.Tree] = parse |> new Namer |> new Typer

  val tac: Task[InputStream, TacProg] = typeCheck |> new TacGen

  val jvm: Task[InputStream, List[JVMClass]] = typeCheck |> new JVMGen

  val optimize: Task[InputStream, TacProg] = tac |> new Optimizer

  val mips: Task[InputStream, String] = optimize |> {
    val emitter = new MipsAsmEmitter
    new Asm(emitter, new BruteRegAlloc(emitter))
  }
}
