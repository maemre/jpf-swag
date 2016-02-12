package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._

sealed trait ExtendedInt
case class RegularInt(i: Int) extends ExtendedInt
case object NegativeInf extends ExtendedInt
case object PositiveInf extends ExtendedInt

// To test some things, run
// $ sbt compile
// $ sbt test
// The test file is in src/test/scala/interval-test.scala
trait AbstractInterval
case object BottomInterval extends AbstractInterval
case class Interval(lower: Option[Int], upper: Option[Int]) extends AbstractInterval {
  // for lower, None represents -∞; for upper, None represents +∞

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

  def +(that: Interval) = {
    Interval(this.lower.flatMap(a ⇒ that.lower.map(a + _)), this.upper.flatMap(a ⇒ that.upper.map(a + _)))
  }

  def -(that: Interval) = {
    Interval(this.lower.flatMap(a ⇒ that.upper.map(a - _)), this.upper.flatMap(a ⇒ that.lower.map(a - _)))
  }

  // See Interval Arithmetic (https://en.wikipedia.org/wiki/Interval_arithmetic) special for division
  def *(that: Interval) = lift2(_*_)(this, that)

  // Possibly keep track of whether div-by-zero would result, add bottom?
  // i.e. when there's a zero in the divisor interval, give bottom instead, or include it?
  // TODO: I forgot that this is the *Integer* interval domain,
  //       so that (2,5) / (-∞,+∞) will result in (2,5), since 2 and 5 can never be divided by a number less than whose absolute value is < |1| 
  def /(that: Interval) = {
    val otherInterval = (that.upper, that.lower) match {
      case (None, None) ⇒  (None, None) // the smallest numbers we can divide by)  // for intervals with 0, we can be even smarter, later... we'd need to create a structure that can keep track of multiple intervals (like [-∞,-4], [2, ∞], etc.)
      case (None, Some(x)) if x >= 0 ⇒  (None, None)
      case (None, Some(x)) ⇒  (None, Some(1.0/x))
      case (Some(x), None) if x <= 0 ⇒  (None, None)
      case (Some(x), None) ⇒  (Some(1.0/x), None)
      case (Some(x), Some(y)) if x == 0 || y == 0 ⇒  (None, None)
      case (Some(x), Some(y)) if x < 0 && y > 0 ⇒  (None, None)
      case (Some(x), Some(y)) ⇒  (Some(1.0/x), Some(1.0/y))
    }
    val lift2option = (x:(Option[Int], Option[Double])) ⇒ x._1.flatMap(a ⇒ x._2.map(a * _))
    val values =
      Seq((this.lower, otherInterval._1),
          (this.lower, otherInterval._2),
          (this.upper, otherInterval._1),
          (this.upper, otherInterval._2)).map(lift2option)
    Interval(values.min.map(_.toInt), values.max.map(_.toInt))  /// can't just do values.min.asInstanceOf[Option[Int]], because Java won't let case from Double to Integer like that
  }

  // TODO, I need to work on this
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

  def createTestInterval(i1: Option[Int], i2: Option[Int]) =
    Interval(i1, i2)
}
