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
import gov.nasa.jpf.symbc.numeric

import edu.ucsb.cs.jpf.swag.helpers._
import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag.domains._
import ImplicitShim._
import Helpers.{ι, addPrime}

import scala.annotation.tailrec
import scala.collection.JavaConversions._

class FixpointListener(config: Config, jpf: JPF) extends PropertyListenerAdapter with PublisherExtension {
  type Domain = NonrelationalDomain[Prefix, AbstractInterval]
  type IP = (Int, Int)
  implicit val intervalFactory = Interval
  implicit val regexFactory = RegexDomain

  val wideningThreshold = Option(config.getString("fixpoint.widening_threshold")).getOrElse("1").toInt
  val methodToAnalyze = config.getString("fixpoint.method")
  val states = MMap[IP, Domain]()
  val wideningCounts = MMap[IP, Int]()
  val lineNumbers = MMap[Int, Int]()
  jpf.addPublisherExtension(classOf[ConsolePublisher], this)

  def joinOrWiden(currPos: IP, state1: Domain, state2: Domain): Domain = {
    if (wideningCounts.getOrElseUpdate(currPos, 0) >= wideningThreshold) {
      // no need to increase the counter since we already exceeded the
      // widening threshold
      state1 ∇ state2
    } else {
      wideningCounts(currPos) = wideningCounts(currPos) + 1
      state1 ⊔ state2
    }
  }

  override def instructionExecuted(vm: VM, thread: ThreadInfo, next: Instruction, insn: Instruction): Unit = {
    if (! insn.getMethodInfo.getLongName.contains(methodToAnalyze)) {
      return // skip
    }
    val pos:IP = (insn.getPosition, next.getPosition)

    lineNumbers += pos._1 → insn.getLineNumber

    // get path condition
    val cg: numeric.PCChoiceGenerator = thread.getVM.getChoiceGenerator match {
      case cg: numeric.PCChoiceGenerator ⇒ cg
      case cg => cg.getPreviousChoiceGeneratorOfType(classOf[numeric.PCChoiceGenerator])
    }

    // there is no path condition so there is nothing to do
    if (cg == null) {
      return
    }

    val pathCondition = Helpers.parsePC(cg.getCurrentPC)

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
                StringValue(Option(obj.asString).map(StringConst(_)).getOrElse(NoStringExpr))
              } else {
                // TODO: create an abstract domain for references for this case
                NumValue(NumConst(sf.getSlot(i)))
              }
            } else {
              NumValue(NumConst(sf.getSlot(i)))
            }
          case e:string.StringExpression ⇒ StringValue(Helpers.parseStrExpr(e))
          case e:string.SymbolicStringBuilder ⇒ StringValue(Option(e.getstr) map Helpers.parseStrExpr getOrElse StringConst(""))
          case e:numeric.IntegerExpression ⇒ NumValue(Helpers.parseNumExpr(e))
        }
    var state: Domain = NonrelationalDomain[Prefix, AbstractInterval]()

    println(s"$pos: Insns\t($insn → $next)")
    println(s"$pos: Stack\t$stack")
    println(s"$pos: PC:\t$pathCondition")
    state.construct(pathCondition, stack)

    println(s"$pos: State\t${state}")
    // project out primes:
    for (i ← state.getVars if i.toString.startsWith("'"))
      state = state.projectOut(i)

    println(s"$pos: After projection\t${state}")
    // println(s"insn $pos $pathCondition")
    if (! states.contains(pos)) {
      states(pos) = state
    } else if (state ⊑ states(pos)) {
      println(s"$pos: Convergence is done.")
      println(s"${Console.MAGENTA}$state${Console.RESET}\n⊑\n${Console.GREEN}${states(pos)}${Console.RESET}\n")
      thread.setTerminated()
      return
    } else {
      states(pos) = joinOrWiden(pos, states(pos), state)
      // TODO: update stack acc. to state
    }

    val (numc, strc) = states(pos).toConstraint.addPrime.toSPFConstraint
    println(s"$pos: General state:\t${Console.GREEN_B + states(pos) + Console.RESET}")
    println(s"$pos: General constraint:\t${Console.BLUE_B + states(pos).toConstraint + Console.RESET}")
    println(Console.CYAN_B + numc + Console.RESET)
    makeStackSymbolic(vm, sf)
    updatePC(cg, numc, strc)
    println(cg.getCurrentPC)
  }

  def makeStackSymbolic(vm:VM, sf:StackFrame): Unit = {
    // update stack to be symbolic
    for (i ← 0 to sf.getTopPos) {
      // TODO: set strings as strings and numbers as numbers
      val symbolicAttr = sf.getSlotAttr(i) match {
        case null ⇒
          if (sf.isReferenceSlot(i)) {
            val slot = sf.getSlot(i)
            val obj = vm.getHeap.get(slot)
            if (obj.isStringObject) {
              new string.StringSymbolic(addPrime(ι(i).toString))
            } else {
              // don't abstract references
              null
            }
          } else {
            // assume that all primitives are integers
            // TODO: change this to support doubles
            new numeric.SymbolicInteger(addPrime(ι(i).toString))
          }
        case e:string.StringExpression ⇒
          new string.StringSymbolic(addPrime(ι(i).toString))
        case e:string.SymbolicStringBuilder ⇒
          new string.SymbolicStringBuilder(new string.StringSymbolic(addPrime(ι(i).toString)))
        case e:numeric.IntegerExpression ⇒
          new numeric.SymbolicInteger(addPrime(ι(i).toString))
      }
      sf.setSlotAttr(i, symbolicAttr)
    }
  }

  def updatePC(cg:numeric.PCChoiceGenerator, numericConstraint: Option[numeric.Constraint], stringConstraint: Option[string.StringConstraint]): Unit = {
    // update path condition
    // TODO: FIX URGENT - parse different cases
    val pc = new numeric.PathCondition()
    numericConstraint foreach { c ⇒
      pc.header = c
    }
    cg.setCurrentPC(pc)
    // TODO: Update string PC too!
  }

  override def publishFinished(publisher: Publisher): Unit = {
    val pw = publisher.getOut

    publisher.publishTopicStart("Abstract States")

    for (insn ← states.keys.toSeq.sorted) {
      // join all states that are seen so far
      pw.println(s"${lineNumbers(insn._1)}: insn$insn: ${states(insn)}")
      pw.println(s"Constraint: ${Console.BLUE}${states(insn).toConstraint}${Console.RESET}")
      pw.println(s"SPF Constraint: ${Console.RED}${states(insn).toConstraint.toSPFConstraint}${Console.RESET}")
    }
  }
}
