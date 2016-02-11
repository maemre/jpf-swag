/**
  * jpf-swag: Sometimes Working Assertion Generator
  */
package edu.ucsb.cs.jpf.swag

import gov.nasa.jpf.{PropertyListenerAdapter, Config, JPF}
import gov.nasa.jpf.report._
import gov.nasa.jpf.vm._
import gov.nasa.jpf.symbc.numeric
import gov.nasa.jpf.symbc.string
import scala.collection.mutable.{Map => MMap, Set => MSet}
import gov.nasa.jpf.jvm.bytecode.{IfInstruction, GOTO, JVMReturnInstruction}
import gov.nasa.jpf.symbc.numeric.solvers.{ProblemGeneral, ProblemZ3}

import edu.ucsb.cs.jpf.swag.helpers._
import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag.domains._
import ImplicitShim._
import ImplicitFactories._

import scala.annotation.tailrec
import scala.collection.JavaConversions._

/**
  * A dummy constraint object to force SPF check all branches?
  */
object DummyConstraint extends numeric.Constraint(null, null, null) {
  def equals(c: numeric.Constraint) = c == DummyConstraint

  def simplify: Boolean = true

  def not = this

  override def postToSolver(pb: ProblemGeneral) = true
}

class FixpointListener(config: Config, jpf: JPF) extends PropertyListenerAdapter with PublisherExtension {
  type Domain = NonrelationalDomain[Prefix, TrivialNumber]

  val methodToAnalyze = "twoifs".toLowerCase
  val states = MMap[Int, Set[Domain]]()
  jpf.addPublisherExtension(classOf[ConsolePublisher], this)

  override def executeInstruction(vm: VM, thread: ThreadInfo, insn: Instruction): Unit = {
    if (! insn.getMethodInfo.getLongName.toLowerCase.contains(methodToAnalyze)) {
      return // skip
    }
    val pos = insn.getPosition

    // get path condition
    val cg: numeric.PCChoiceGenerator = thread.getVM.getChoiceGenerator match {
      case cg: numeric.PCChoiceGenerator ⇒ cg
      case cg => cg.getPreviousChoiceGeneratorOfType(classOf[numeric.PCChoiceGenerator])
    }
    val pathCondition = Option(cg).map(cg ⇒ Helpers.parsePC(cg.getCurrentPC)).getOrElse(constraints.Conjunction())

    // get values from stack
    // TODO: It turns out that this doesn't work without a stack
    val sf = thread.getTopFrame // stack frame
    val stack: IndexedSeq[StackValue] =
      for (i ← 0 to sf.getTopPos) yield
        sf.getSlotAttr(i) match {
          case null ⇒
            if (sf.isReferenceSlot(i)) {
              // TODO: create an abstract domain for references for this case
              NumValue(NumConst(sf.getSlot(i)))
            } else {
              NumValue(NumConst(sf.getSlot(i)))
            }
          case e:string.StringExpression ⇒ StringValue(Helpers.parseStrExpr(e))
          case e:string.SymbolicStringBuilder ⇒ StringValue(Option(e.getstr) map Helpers.parseStrExpr getOrElse NoStringExpr)
          case e:numeric.IntegerExpression ⇒ NumValue(Helpers.parseNumExpr(e))
        }
    val state: Domain = NonrelationalDomain[Prefix, TrivialNumber]()
    state.construct(pathCondition, stack)

    // Alternative strategy:
    // Collect all states, then merge paths later on
    if (! states.contains(pos)) {
      states(pos) = Set(state)
    } else {
      states(pos) = states(pos) + state
    }

    /**
    if (! states.contains(pos)) {
      states = states + (pos → state)
    } else if (state ⊑ states(pos)) {
      thread.skipInstruction(insn.getNext)
    } else {
      states = states + (pos → (states(pos) ⊔ state))
      // TODO: update stack acc. to state
    }
      */
  }

  override def publishFinished(publisher: Publisher): Unit = {
    val pw = publisher.getOut

    publisher.publishTopicStart("Path conditions")

    for (insn ← states.keys.toSeq.sorted) {
      // join all states that are seen so far
      val state = states(insn).reduce(_ ⊔ _)
      // project to first value on stack
      val constraint = state.projectTo(0)
      pw.println(s"$insn:\t${constraint}")
    }
  }
}
