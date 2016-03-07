package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag.helpers._

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
  def replace(m: T, r: T): T
  def replaceAll(m: T, r: T): T
  def replaceFirst(m: T, r: T): T
  def trim: T
  def substring(i: NumExpr): T
  // generate a constraint describing length where length is named as `v`
  def length(v: String): Constraint
  def toLowerCase: T
  def toUpperCase: T
  def indexOf(c: CharExpr): NumExpr
  def lastIndexOf(c: CharExpr): NumExpr
}

trait AbstractNumber[T] extends AbstractDomain[T] {
  def +(that: T): T
  def -(that: T): T
  def *(that: T): T
  def /(that: T): T
  def %(that: T): T

  def addConstraint(cmp:NumComparator.NumComparator, rhs:T): T
}

trait CompositeAbstractDomain[T] {
  def construct(pathCondition: Constraint, stack: IndexedSeq[StackValue]): T

  /**
    * Compute transition from this state to next state as described with
    * `pathCondition` and `stack`. old values of stack variables are denoted
    * with `stack_ID` where `ID` is the index on stack.
    */
  def transit(pathCondition: Constraint, stack: IndexedSeq[StackValue]): T

  def ⊔(that: T): T

  def ⊑(that: T): Boolean

  def ∇(that: T): T

  def projectTo(stackIdx: Int): Option[Constraint]
  def projectTo(name: String): Option[Constraint]

  def projectOut(stackIdx: Int): T
  def projectOut(name: String): T

  def toConstraint: Constraint

  def getVars: Set[ID]
}

trait RelationalString[T] extends AbstractDomain[T] {
  def construct(constraints: Conjunction): T

  def projectTo(id: ID): Option[Constraint]
  
  def projectOut(id: ID): T

  // Get length constraint over a variable
  // used for communicating length back-and-forward
  def lengthConstraint(id: ID): T

  // Constraint representation of this abstract domain
  def toConstraint: Constraint
}

trait RelationalNumber[T] extends AbstractDomain[T] {
  def construct(constraints: Conjunction): T

  def projectTo(id: ID): Option[Constraint]

  def projectOut(id: ID): T

  // add extra constraint
  def addConstraint(c: Constraint): T
  
  // Constraint representation of this abstract domain
  def toConstraint: Constraint
}

trait AbstractDomainFactory[T <: AbstractDomain[T]] {
  def ⊥ : T
  def ⊤ : T
}

trait AbstractStringFactory[T <: AbstractString[T]] extends AbstractDomainFactory[T] {
  def const(s: String): T
  def valueOf(n: NumExpr): T
}

trait AbstractNumberFactory[T <: AbstractNumber[T]] extends AbstractDomainFactory[T] {
  def const(s: Int): T
}

trait CompositeAbstractDomainFactory[T <: CompositeAbstractDomain[T]] {
  def ⊥ : T
  def ⊤ : T
}
