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
import edu.ucsb.cs.jpf.swag.domains._

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
  var states = Map[Int, Domain]()
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

    // get values from stackx
    val stack: IndexedSeq[StackValue] = thread.getTopFrame.operandAttrIterator.toIndexedSeq map {
      case e:string.StringExpression ⇒ StringValue(Helpers.parseStrExpr(e))
      case e:numeric.IntegerExpression ⇒ NumValue(Helpers.parseNumExpr(e))
    }

    val state: Domain = ???
    state.construct(pathCondition, stack)

    if (! states.contains(pos)) {
      states = states + (pos → state)
    } else if (state ⊑ states(pos)) {
      thread.skipInstruction(insn.getNext)
    } else {
      states = states + (pos → (states(pos) ⊔ state))
      // TODO: update stack acc. to state
    }
  }

  override def publishFinished(publisher: Publisher): Unit = {
    val pw = publisher.getOut

    publisher.publishTopicStart("Path conditions")

    for (insn ← states.keys.toSeq.sorted) {
      // project to first value on stack
      val constraint = states(insn).projectTo(0)
      pw.println(s"$insn:\t${constraint}")
    }
  }
}
