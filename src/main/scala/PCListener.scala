/**
  * jpf-swag: Sometimes Working Assertion Generator
  */
package edu.ucsb.cs.jpf.swag

import gov.nasa.jpf.{PropertyListenerAdapter, Config, JPF}
import gov.nasa.jpf.report._
import gov.nasa.jpf.vm._
import gov.nasa.jpf.symbc.numeric._
import scala.collection.mutable.{Map => MMap, Set => MSet}
import gov.nasa.jpf.jvm.bytecode.{IfInstruction, GOTO, JVMReturnInstruction}
import gov.nasa.jpf.symbc.numeric.solvers.{ProblemGeneral, ProblemZ3}
import edu.ucsb.cs.jpf.swag.helpers._

import scala.annotation.tailrec

class PCListener(config: Config, jpf: JPF) extends PropertyListenerAdapter with PublisherExtension {

  val methodToAnalyze = config.getString("fixpoint.method")
  val PCs: MMap[String, MMap[Int, MSet[constraints.Constraint]]] = MMap()
  jpf.addPublisherExtension(classOf[ConsolePublisher], this)

  def getPC(insn: Instruction) = {
    val name = insn.getMethodInfo.getLongName
    val pos = insn.getPosition
    //println("In getPC")
    if (PCs.get(name).isEmpty) {
      //println(s"$insn is empty")
      PCs(name) = MMap()
    }
    if (PCs(name).get(pos).isEmpty) {
      //println(s"$insn at $pos is empty")
      PCs(name)(pos) = MSet()
    }
    //println(s"Leaving getPC, returning ${PCs(name)(pos)} at $pos")
    PCs(name)(pos)
  }

  def addCurrentPC(insn: Instruction, thread: ThreadInfo): Unit = {
    val cg: PCChoiceGenerator = thread.getVM.getChoiceGenerator match {
      case cg: PCChoiceGenerator ⇒ cg
      case cg => cg.getPreviousChoiceGeneratorOfType(classOf[PCChoiceGenerator])
    }

    val pcs = getPC(insn) // call for side-effect
    Option(cg) foreach { cg ⇒
      val pc = cg.getCurrentPC.make_copy
      val constraint = Helpers.parsePC(pc)
      println(s"${insn.getPosition}: CONSTRAINT: $constraint")
      pcs += constraint
      // val oldConstraint = getPC(insn)
      // val newConstraint = oldConstraint | pc.header
      // if (newConstraint ⊑ oldConstraint) {
      //   val pos = insn.getPosition
      //   println(s"----------------- $pos Reached fixpoint: $oldConstraint")
      // } else {
      //   pc.header = newConstraint
      //   //cg.setCurrentPC(pc)
      // }
    }
  }

  override def executeInstruction(vm: VM, thread: ThreadInfo, insn: Instruction): Unit = {
    if (! insn.getMethodInfo.getLongName.toLowerCase.contains(methodToAnalyze)) {
      return // skip
    }
    addCurrentPC(insn, thread)
    val sf = thread.getTopFrame // stack frame
    //println("Executing instruction(s):")
    // for (i ← 0 to sf.getTopPos) {
    //   if (sf.isReferenceSlot(i)) {
    //     println(s"${insn.getPosition}:\t$i → ${vm.getHeap.get(sf.getSlot(i))} ${vm.getHeap.get(sf.getSlot(i)).isStringObject}")
    //   } else {
    //     println(s"Ins#${insn.getPosition}:\t StackItem#$i → ${sf.getSlot(i)}")
    //   }
    // }
//    val pc = new PathCondition()
//    pc.header = getPC(insn)
//    println(s"simplifying ${pc.header}")
//    println(pc.simplify())
  }

  override def instructionExecuted(vm: VM, thread: ThreadInfo, next: Instruction, insn: Instruction): Unit = {
    if (! insn.getMethodInfo.getLongName.toLowerCase.contains(methodToAnalyze)) {
      return // skip
    }
    println(s"Ins executed: ${insn.getPosition}:\t${getPC(insn)}")
  }

  override def publishFinished(publisher: Publisher): Unit = {
    val pw = publisher.getOut

    publisher.publishTopicStart("Path conditions")

    for ((mname, pcs) <- PCs) {
      if (mname.toLowerCase.contains(methodToAnalyze)) {
        pw.println(s"Path conditions for ${mname}")
        for (insn <- pcs.keys.toSeq.sorted) {
          val ors = pcs(insn).map(_.toString).mkString(" ")
          pw.println(s"$insn:\t(or $ors)")
        }
      }
    }
  }
}
