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
}

object ImplicitFactories {
  implicit object TrivialNumberFactory extends AbstractNumberFactory[TrivialNumber] {
    val top = TrivialNumber(true)
    val bottom = TrivialNumber(false)
    def const(n: Int) = top
  }
}

import ImplicitFactories._
