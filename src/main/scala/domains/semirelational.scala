package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag.helpers._
import scala.annotation.tailrec
import scala.collection.mutable.{Map ⇒ MMap, Set ⇒ MSet, StringBuilder}

import Helpers.ι

case class SemirelationalDomain[Str <: RelationalString[Str], Num <: RelationalNumber[Num]](
  var str: Str,
  var num: Num
)
  /*(
  strTop: ⇒ Str,
  numTop: ⇒ Num
)*/ extends CompositeAbstractDomain[SemirelationalDomain[Str, Num]] {
  def construct(pathCondition: Constraint, stack: IndexedSeq[StackValue]) = {
    val numConstraints = MSet[Constraint](pathCondition)
    val strConstraints = MSet[Constraint](pathCondition)
    // construct conditions for stack
    for ((s, i) ← stack.zipWithIndex) s match {
      case NumValue(e) ⇒
        numConstraints += NumVar(StackID(i).toString) ≡ e
      case StringValue(e) ⇒
        strConstraints += StringVar(StackID(i).toString) ≡ e
    }
    num = num.construct(Conjunction(numConstraints.toSet))
    str = str.construct(Conjunction(strConstraints.toSet))
    // TODO: iteratively communicate length constraints for extra precision
    this
  }

  def transit(pathCondition: Constraint, stack: IndexedSeq[StackValue]) = ???
  // {
  //   // Symbols are from "PostHat and All That"
  //   // γ^(a)
  //   val oldState = this.toConstraint.addPrime
  //   // φ_τ
  //   val transition = MSet[Constraint](pathCondition.addPrime)
  //   for ((s, i) ← stack.zipWithIndex) s match {
  //     case NumValue(e) ⇒
  //       transition += (NumVar(StackID(i).toString) ≡ e).addPrime
  //     case StringValue(e) ⇒
  //       transition += (StringVar(StackID(i).toString) ≡ e).addPrime
  //   }
  //   val φ_τ = Conjunction(transition.toSet)
  //   val primed = SemirelationalDomain[Str, Num](strTop, numTop).construct(oldState & φ_τ, IndexedSeq())
  //   ??? // primed.projectToPrimes.toConstraint.removePrime
  // }

  def projectToPrimes: SemirelationalDomain[Str, Num] = ???

  def ⊔(that: SemirelationalDomain[Str, Num]) = {
    // TODO: should we communicate the lengths?
    SemirelationalDomain(this.str ⊔ that.str, this.num ⊔ that.num)
  }

  def ⊑(that: SemirelationalDomain[Str, Num]): Boolean = this.num ⊑ that.num && this.str ⊑ that.str

  def ∇(that: SemirelationalDomain[Str, Num]) = {
    SemirelationalDomain(this.str ∇ that.str, this.num ∇ that.num)
  }

  def projectTo(id: Int): Option[Constraint] = projectTo(StackID(id))
  def projectTo(name: String): Option[Constraint] = projectTo(Name(name))

  def projectTo(id: ID): Option[Constraint] = {
    val nc = num.projectTo(id)
    val sc = str.projectTo(id)
    // extract nc and sc to compute conjunction, if one is missing return other
    nc.map(c ⇒ sc.map(c & _).getOrElse(c)).orElse(sc)
  }

  def projectOut(id: Int): SemirelationalDomain[Str, Num] = projectOut(StackID(id))
  def projectOut(name: String): SemirelationalDomain[Str, Num] = projectOut(Name(name))

  def projectOut(id: ID): SemirelationalDomain[Str, Num] = SemirelationalDomain(str.projectOut(id), num.projectOut(id))

  // TODO: add constraint propagation (here maybe)
  def toConstraint: Constraint = num.toConstraint & str.toConstraint

  override def toString = s"SemirelationalDomain(\n$num\n$str)"

  def getVars = ???
}
