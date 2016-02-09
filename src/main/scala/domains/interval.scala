package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._

sealed trait ExtendedInt
case class RegularInt(i: Int) extends ExtendedInt
case object NegativeInf extends ExtendedInt
case object PositiveInf extends ExtendedInt

trait AbstractInterval
case object BottomInterval extends AbstractInterval
case class Interval(lower: Option[Int], upper: Option[Int]) extends AbstractInterval {

  def lift2(op: (Int, Int) ⇒ Int): (Interval, Interval) ⇒ Interval = { (i, j) ⇒
    val lift2option = (x:(Option[Int], Option[Int])) ⇒ x._1.flatMap(a ⇒ x._2.map(op(a,_)))
    val values = Seq((i.lower, j.lower), (i.lower, j.upper), (i.upper, j.lower), (i.upper, j.upper)).map(lift2option)
    Interval(values.min, values.max)
  }

  def ⊔(that: Interval) = Interval(this.lower.flatMap(a ⇒ that.lower.map(a min _)), this.upper.flatMap(a ⇒ that.upper.map(a max _)))

  def ⊑(that: Interval) = {
    (this.lower, that.lower) match {
      case (None, Some(_)) ⇒ false
      case (Some(a), Some(b)) if a < b ⇒ false
      case _ ⇒
        (this.upper, that.upper) match {
          case (None, Some(_)) ⇒ false
          case (Some(a), Some(b)) ⇒ a <= b
          case _ ⇒ true
        }
    }
  }

  // TODO: Revise operations
  def +(that: Interval) = {
    Interval(this.lower.flatMap(a ⇒ that.lower.map(a + _)), this.upper.flatMap(a ⇒ that.upper.map(a + _)))
  }

  def -(that: Interval) = {
    Interval(this.lower.flatMap(a ⇒ that.upper.map(a - _)), this.upper.flatMap(a ⇒ that.lower.map(a - _)))
  }

  def *(that: Interval) = {
    Interval(this.lower.flatMap(a ⇒ that.lower.map(a * _)), this.upper.flatMap(a ⇒ that.upper.map(a * _)))
  }

  def /(that: Interval) = lift2(_/_)(this, that)

  def %(that: Interval) = lift2(_/_)(this, that)

  override def toString = {
    val low = lower.map(_.toString).getOrElse("-∞")
    val high = upper.map(_.toString).getOrElse("∞")
    s"Interval(${low}, ${high})"
  }
}

object Interval {
  def apply(c: Int): Interval = Interval(Some(c), Some(c))
  val ⊤ = Interval(None, None)
  val ⊥ = BottomInterval
}
