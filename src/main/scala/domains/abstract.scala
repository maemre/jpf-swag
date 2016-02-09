package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._

sealed trait StackValue
case class NumValue(e: NumExpr) extends StackValue
case class StringValue(e: StringExpr) extends StackValue

trait AbstractDomain[T] {
  def ⊔(that: T): T
  def ∇(that: T) = this ⊔ that
  def ⊑(that: T): Boolean
  def toConstraint(v: String): Constraint
}

trait AbstractString[T] extends AbstractDomain[T] {
  def concat(s: T): T
}

trait AbstractNumber[T] extends AbstractDomain[T] {
  def +(that: T): T
  def -(that: T): T
  def *(that: T): T
  def /(that: T): T
  def %(that: T): T
}

case class AbstractBoolean(b: Set[Boolean]) extends AbstractDomain[AbstractBoolean] {
  def toConstraint(v: String) =
    if (b == Set(true))
      True
    else if (b == Set(false))
      False
    else if (b == Set())
      Conjunction()
    else
      BoolVar(v)

  def ⊔(that: AbstractBoolean) = AbstractBoolean(this.b ++ that.b)
  def ⊑(that: AbstractBoolean) = this.b subsetOf that.b
}

object AbstractBoolean {
  def apply(b: Boolean*): AbstractBoolean = AbstractBoolean(b.toSet)
  def top = AbstractBoolean(true, false)
  def bot = AbstractBoolean()
}

trait CompositeAbstractDomain[T] {
  def construct(pathCondition: Constraint, stack: IndexedSeq[StackValue]): Unit

  def ⊔(that: T): T

  def ⊑(that: T): Boolean

  def ∇(that: T): T

  def projectTo(stackIdx: Int): Option[Constraint]
  def projectTo(name: String): Option[Constraint]

  def projectOut(stackIdx: Int): T
  def projectOut(name: String): T
}

trait CompositeAbstractDomainFactory[T] {
  def bottom: CompositeAbstractDomain[T]
  def top: CompositeAbstractDomain[T]
}
