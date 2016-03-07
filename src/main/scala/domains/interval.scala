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
trait AbstractInterval extends AbstractNumber[AbstractInterval] {
}
case object BottomInterval extends AbstractInterval {
  // Members declared in AbstractDomain
  def ⊑(that: AbstractInterval): Boolean = true
  def ⊔(that: AbstractInterval): AbstractInterval = that
  def toConstraint(v: String): Constraint = False
  
  // Members declared in AbstractNumber
  def /(that: AbstractInterval): AbstractInterval = BottomInterval
  def -(that: AbstractInterval): AbstractInterval = BottomInterval
  def %(that: AbstractInterval): AbstractInterval = BottomInterval
  def +(that: AbstractInterval): AbstractInterval = BottomInterval
  def *(that: AbstractInterval): AbstractInterval = BottomInterval

  def addConstraint(cmp: NumComparator.NumComparator, rhs: AbstractInterval) = BottomInterval
}
case class Interval(lower: Option[Int], upper: Option[Int]) extends AbstractInterval {
  // for lower, None represents -∞; for upper, None represents +∞

  def lift2(op: (Int, Int) ⇒ Int): (Interval, Interval) ⇒ Interval = { (i, j) ⇒
    val lift2option = (x:(Option[Int], Option[Int])) ⇒ x._1.flatMap(a ⇒ x._2.map(op(a,_)))
    val values = Seq((i.lower, j.lower), (i.lower, j.upper), (i.upper, j.lower), (i.upper, j.upper)).map(lift2option)
    Interval(values.min, values.max)
  }

  def ⊔(that: AbstractInterval) = that match {
    case that:Interval ⇒
      Interval(this.lower.flatMap(a ⇒ that.lower.map(a min _)), this.upper.flatMap(a ⇒ that.upper.map(a max _)))
    case BottomInterval ⇒
      this
  }

  def ⊑(that: AbstractInterval) = that match {
    case that: Interval ⇒
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
    case _ ⇒ false
  }

  def +(that: AbstractInterval) = that match {
    case that:Interval ⇒
      Interval(this.lower.flatMap(a ⇒ that.lower.map(a + _)), this.upper.flatMap(a ⇒ that.upper.map(a + _)))
    case _ ⇒ BottomInterval
  }

  def -(that: AbstractInterval) = that match {
    case that:Interval ⇒
      Interval(this.lower.flatMap(a ⇒ that.upper.map(a - _)), this.upper.flatMap(a ⇒ that.lower.map(a - _)))
    case _ ⇒ BottomInterval
  }

  // See Interval Arithmetic (https://en.wikipedia.org/wiki/Interval_arithmetic) special for division
  def *(that: AbstractInterval) = that match {
    case that: Interval ⇒ lift2(_*_)(this, that)
    case _ ⇒ BottomInterval
  }

  // Possibly keep track of whether div-by-zero would result, add bottom?
  // i.e. when there's a zero in the divisor interval, give bottom instead, or include it?
  // TODO: I forgot that this is the *Integer* interval domain,
  //       so that (2,5) / (-∞,+∞) will result in (2,5), since 2 and 5 can never be divided by a number less than whose absolute value is < |1| 
  def /(that: AbstractInterval) = that match {
    case that: Interval ⇒
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
    case _ ⇒
      BottomInterval
  }

  // TODO, I need to work on this
  def %(that: AbstractInterval) = that match {
    case that: Interval ⇒ lift2(_%_)(this, that)
    case _ ⇒ BottomInterval
  }

  override def toString = {
    val low = lower.map(_.toString).getOrElse("-∞")
    val high = upper.map(_.toString).getOrElse("∞")
    s"Interval(${low}, ${high})"
  }

  def toConstraint(v: String) = {
    val x = NumVar(v)
    (lower, upper) match {
      case (Some(lower), Some(upper)) ⇒
        (x ≥ NumConst(lower)) & (x ≤ NumConst(upper))
      case (None, Some(upper)) ⇒
        x ≤ NumConst(upper)
      case (Some(lower), None) ⇒
        x ≥ NumConst(lower)
      case (None, None) ⇒
        True // TODO: we need to describe that ∃x somehow
    }
  }

  def ⊓(that: Interval) = {
    val l = (lower, that.lower) match {
      case (Some(a), Some(b)) ⇒ Some(a max b)
      case (Some(a), None) ⇒ Some(a)
      case (None, Some(a)) ⇒ Some(a)
      case (None, None) ⇒ None
    }
    val u = (upper, that.upper)  match {
      case (Some(a), Some(b)) ⇒ Some(a min b)
      case (Some(a), None) ⇒ Some(a)
      case (None, Some(a)) ⇒ Some(a)
      case (None, None) ⇒ None
    }
    Interval(l, u).checkForBottom
  }

  // add extra constraint
  def addConstraint(cmp: NumComparator.NumComparator, rhs: AbstractInterval) = rhs match {
    case BottomInterval if cmp == NumComparator.≠ ⇒ this
    case BottomInterval ⇒ BottomInterval
    case that:Interval ⇒
      cmp match {
        case NumComparator.≡ ⇒ this ⊓ that
        case NumComparator.≠ ⇒ 
          // approximate by using the fact x ≠ y ⇔ x < y ∨ x > y
          addConstraint(NumComparator.<, that) ⊔ addConstraint(NumComparator.>, that)
        case NumComparator.≤ ⇒
          val f: PartialFunction[(Option[Int], Option[Int]), Int] = {
            case (Some(u), Some(v)) ⇒ u min v
            case (Some(u), None)    ⇒ u
            case (None, Some(v))    ⇒ v
          }
          copy(upper=f lift (upper → that.upper)).checkForBottom
        case NumComparator.≥ ⇒
          val f: PartialFunction[(Option[Int], Option[Int]), Int] = {
            case (Some(u), Some(v)) ⇒ u max v
            case (Some(u), None)    ⇒ u
            case (None, Some(v))    ⇒ v
          }
          copy(lower=f lift (lower → that.lower)).checkForBottom
        case NumComparator.< ⇒
          val f: PartialFunction[(Option[Int], Option[Int]), Int] = {
            case (Some(u), Some(v)) ⇒ u min (v-1)
            case (Some(u), None)    ⇒ u
            case (None, Some(v))    ⇒ v
          }
          copy(upper=f lift (upper → that.upper)).checkForBottom
        case NumComparator.> ⇒
          val f: PartialFunction[(Option[Int], Option[Int]), Int] = {
            case (Some(u), Some(v)) ⇒ u max (v+1)
            case (Some(u), None)    ⇒ u
            case (None, Some(v))    ⇒ v
          }
          copy(lower=f lift (lower → that.lower)).checkForBottom
      }
  }

  def checkForBottom: AbstractInterval = lower.flatMap(l ⇒ upper.map(u ⇒ if (u < l) BottomInterval else this)).getOrElse(this)

  override def ∇(that: AbstractInterval) = that match {
    case that:Interval ⇒ {
      val l = for (
        l1 ← lower;
        l2 ← that.lower;
        if l1 <= l2) yield l1
      
      val u = for (
        u1 ← upper;
        u2 ← that.upper;
        if u1 >= u2) yield u1

      Interval(l, u)
    }
    case BottomInterval ⇒ this
  }
}

object Interval extends AbstractNumberFactory[AbstractInterval] {
  def apply(c: Int): Interval = Interval(Some(c), Some(c))
  val ⊤ = Interval(None, None)
  val ⊥ = BottomInterval
  def const(c: Int) = apply(c)

  def createTestInterval(i1: Option[Int], i2: Option[Int]) =
    Interval(i1, i2)
}
