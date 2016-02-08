package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._

sealed trait StackValue
case class NumValue(e: NumExpr) extends StackValue
case class StringValue(e: StringExpr) extends StackValue

trait CompositeAbstractDomain {
  def construct(pathCondition: Constraint, stack: IndexedSeq[StackValue]): Unit

  def ⊔(that: CompositeAbstractDomain): CompositeAbstractDomain

  def ⊑(that: CompositeAbstractDomain): Boolean

  def ∇(that: CompositeAbstractDomain): CompositeAbstractDomain

  def projectTo(stackIdx: Int): Constraint
  def projectTo(name: String): Constraint

  def projectOut(stackIdx: Int): CompositeAbstractDomain
  def projectOut(name: String): CompositeAbstractDomain
}

trait CompositeAbstractDomainFactory {
  def bottom: CompositeAbstractDomain
  def top: CompositeAbstractDomain
}
