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

case class ProperConstraint(left: Expression, comp: Comparator, right: Expression) extends Constraint(left, comp, right) {

  def eqchk[A](l:A, r:A) = if (l == null) r == null else l.equals(r)

  override def equals(o: Any): Boolean = o match {
    case that:Constraint ⇒
      println(s"------------------ $this == $that")
      eqchk(left, that.getLeft) && eqchk(comp, that.getComparator) && eqchk(right, that.getRight)
    case _ ⇒ false
  }

  override def not = ???
}

object ProperConstraint {
  def apply(c: Constraint): ProperConstraint = ProperConstraint(c.getLeft, c.getComparator, c.getRight)
}

case class DisjunctiveConstraint(val disjuncts: MSet[Constraint], val neg: Boolean = false) extends Constraint(null, null, null) {
  override def toString = if (disjuncts.isEmpty) "⊥" else disjuncts.mkString(" ∨ ")

  def not: DisjunctiveConstraint = /*{
    val result = disjuncts.map(_.not) reduceOption { (a: Constraint, b: Constraint) ⇒
      a.and = b
      a
    }
    result.getOrElse(null) // TODO: should return true
  }*/ copy(neg= !neg) // this might not play well with MSet[Constraint]

  def |=(c: Constraint): DisjunctiveConstraint = c match {
    case c: DisjunctiveConstraint ⇒
      this |= c
    case _ ⇒
      if (c != null) {
        println(s"adding $c to $disjuncts")
        println(c.getLeft)
        println(c.getClass)
        disjuncts += c
      }
      this
  }

  def |(c: Constraint): DisjunctiveConstraint = {
    DisjunctiveConstraint() |= this |= c
  }

  def |=(c: DisjunctiveConstraint): DisjunctiveConstraint = {
    disjuncts ++= c.disjuncts
    this
  }

  def ⊑(that: DisjunctiveConstraint): Boolean = {
    // to check validity of (that ⇒ this) = (¬ that ∨ this), we will
    // check satisfiability of its negation: that ∧ ¬ this

    // TODO: simplify with conjunction-like stuff
    // println(this.prefix_notation)
    false
    // TODO: give the whole condition at once?
    //val composite = Seq(this.not, that).map((c:Constraint) ⇒ ConstraintSolver.parseSMTLIB2String(s"(assert ${c.prefix_notation})"))
    //! ConstraintSolver.solveConstraints(composite)
  }

  override def prefix_notation: String = {
    // TODO: add conjunctions
    val core = if (disjuncts.isEmpty) {
      "false"
    } else {
      "(or " + disjuncts.map(_.prefix_notation).mkString(" ") + ")"
    }
    if (neg) s"(not $core)" else core
  }
}

object DisjunctiveConstraint {
  def apply(): DisjunctiveConstraint = DisjunctiveConstraint(MSet())
}

class PCListener(config: Config, jpf: JPF) extends PropertyListenerAdapter with PublisherExtension {

  val methodToAnalyze = "foo".toLowerCase
  val PCs: MMap[String, MMap[Int, DisjunctiveConstraint]] = MMap()
  jpf.addPublisherExtension(classOf[ConsolePublisher], this)

  def getPC(insn: Instruction) = {
    val name = insn.getMethodInfo.getLongName
    val pos = insn.getPosition
    if (PCs.get(name).isEmpty) {
      PCs(name) = MMap()
    }
    if (PCs(name).get(pos).isEmpty) {
      PCs(name)(pos) = DisjunctiveConstraint() // MSet() // new PathCondition()
    }
    PCs(name)(pos)
  }

  def addCurrentPC(insn: Instruction, thread: ThreadInfo): Unit = {
    val cg: PCChoiceGenerator = thread.getVM.getChoiceGenerator match {
      case cg: PCChoiceGenerator ⇒ cg
      case cg => cg.getPreviousChoiceGeneratorOfType(classOf[PCChoiceGenerator])
    }

    val pc = getPC(insn) // call for side-effect
    Option(cg) foreach { cg ⇒
      val pc = cg.getCurrentPC.make_copy
      val constraint = Helpers.parsePC(pc)
      println(s"${insn.getPosition}: CONSTRAINT: $constraint")
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
    for (i ← 0 to sf.getTopPos) {
      if (sf.isReferenceSlot(i)) {
        println(s"${insn.getPosition}:\t$i → ${vm.getHeap.get(sf.getSlot(i))} ${vm.getHeap.get(sf.getSlot(i)).isStringObject}")
      } else {
        println(s"${insn.getPosition}:\t$i → ${sf.getSlot(i)}")
      }
    }
//    val pc = new PathCondition()
//    pc.header = getPC(insn)
//    println(s"simplifying ${pc.header}")
//    println(pc.simplify())
  }

  override def instructionExecuted(vm: VM, thread: ThreadInfo, next: Instruction, insn: Instruction): Unit = {
    if (! insn.getMethodInfo.getLongName.toLowerCase.contains(methodToAnalyze)) {
      return // skip
    }
    print(s"${insn.getPosition}:\t")
    println(getPC(insn))
  }

  override def publishFinished(publisher: Publisher): Unit = {
    val pw = publisher.getOut

    publisher.publishTopicStart("Path conditions")

    for ((mname, pcs) <- PCs) {
      if (mname.toLowerCase.contains(methodToAnalyze)) {
        pw.println(s"Path conditions for ${mname}")
        for (insn <- pcs.keys.toSeq.sorted) {
          pw.println(s"$insn:\t${pcs(insn).prefix_notation}")
        }
      }
    }
  }
}
