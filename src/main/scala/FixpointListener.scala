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

class FixpointListener(config: Config, jpf: JPF) extends PropertyListenerAdapter with PublisherExtension {
  type Domain = NonrelationalDomain[Prefix, AbstractInterval]
  implicit val intervalFactory = Interval

  val wideningThreshold = 10
  val methodToAnalyze = config.getString("fixpoint.method")
  val states = MMap[Int, Domain]()
  val wideningCounts = MMap[Int, Int]()
  val lineNumbers = MMap[Int, Int]()
  jpf.addPublisherExtension(classOf[ConsolePublisher], this)

  def joinOrWiden(currPos: Int, state1: Domain, state2: Domain): Domain = {
    if (wideningCounts.getOrElseUpdate(currPos, 0) >= wideningThreshold) {
      // no need to increase the counter since we already exceeded the
      // widening threshold
      state1 ∇ state2
    } else {
      wideningCounts(currPos) = wideningCounts(currPos) + 1
      state1 ⊔ state2
    }
  }

  override def executeInstruction(vm: VM, thread: ThreadInfo, insn: Instruction): Unit = {
    if (! insn.getMethodInfo.getLongName.contains(methodToAnalyze)) {
      return // skip
    }
    val pos = insn.getPosition

    lineNumbers += pos → insn.getLineNumber

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
              // extract strings
              val slot = sf.getSlot(i)
              val obj = vm.getHeap.get(slot)
              if (obj.isStringObject) {
                StringValue(StringConst(obj.asString))
              } else {
                // TODO: create an abstract domain for references for this case
                NumValue(NumConst(sf.getSlot(i)))
              }
            } else {
              NumValue(NumConst(sf.getSlot(i)))
            }
          case e:string.StringExpression ⇒ StringValue(Helpers.parseStrExpr(e))
          case e:string.SymbolicStringBuilder ⇒ StringValue(Option(e.getstr) map Helpers.parseStrExpr getOrElse NoStringExpr)
          case e:numeric.IntegerExpression ⇒ NumValue(Helpers.parseNumExpr(e))
        }
    val state: Domain = NonrelationalDomain[Prefix, AbstractInterval]()
    state.construct(pathCondition, stack)

    // println(s"$pos: Stack\t$stack")
    // println(s"$pos: PC\t$pathCondition")
    // println(s"$pos: State\t${state.i2n}")
    if (! states.contains(pos)) {
      states(pos) = state
    } else if (state ⊑ states(pos)) {
      // thread.setTerminated()
    } else {
      states(pos) = joinOrWiden(pos, states(pos), state)
      // TODO: update stack acc. to state
    }
    
  }

  override def publishFinished(publisher: Publisher): Unit = {
    val pw = publisher.getOut

    publisher.publishTopicStart("Abstract States")

    for (insn ← states.keys.toSeq.sorted) {
      // join all states that are seen so far
      pw.println(s"${lineNumbers(insn)}: insn$insn: ${states(insn)}")
    }
  }
}
