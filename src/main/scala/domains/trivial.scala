package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._

case class TrivialNumber(isTop: Boolean) extends AbstractNumber[TrivialNumber] {
  def +(that: TrivialNumber) = copy(this.isTop && that.isTop)
  def -(that: TrivialNumber) = copy(this.isTop && that.isTop)
  def *(that: TrivialNumber) = copy(this.isTop && that.isTop)
  def /(that: TrivialNumber) = copy(this.isTop && that.isTop)
  def %(that: TrivialNumber) = copy(this.isTop && that.isTop)

  def ⊔(that: TrivialNumber) = copy(this.isTop || that.isTop)
  def ⊑(that: TrivialNumber) = !this.isTop || that.isTop
  def toConstraint(v: String) = True

  def addConstraint(cmp: NumComparator.NumComparator, rhs: TrivialNumber) = copy(this.isTop && rhs.isTop)

  override def toString = if (isTop) "⊤" else "⊥"
}

object ImplicitFactories {
  implicit object TrivialNumberFactory extends AbstractNumberFactory[TrivialNumber] {
    val ⊤ = TrivialNumber(true)
    val ⊥ = TrivialNumber(false)
    def const(n: Int) = ⊤
  }
}

import ImplicitFactories._
