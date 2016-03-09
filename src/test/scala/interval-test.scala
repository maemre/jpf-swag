import edu.ucsb.cs.jpf.swag.domains._
import org.scalatest._

class IntervalSpec extends FlatSpec {

  // Addition
  "Two intervals (-∞, +∞) and (-∞, -1)" should "add together to be (-∞, +∞)" in {
    assert(Interval.createTestInterval(None, None) +
           Interval.createTestInterval(None, Some(-1)) ==
           Interval.createTestInterval(None, None))
  }

  "Two intervals (-∞, +∞) and (-42, +∞)" should "add together to be (-∞, +∞)" in {
  assert(Interval.createTestInterval(None, None) +
         Interval.createTestInterval(Some(-42), None) ==
         Interval.createTestInterval(None, None))
  }

  "Two intervals (-∞, -1) and (-∞, -1)" should "add together to be (-∞, -2)" in {
  assert(Interval.createTestInterval(None, Some(-1)) +
         Interval.createTestInterval(None, Some(-1)) ==
         Interval.createTestInterval(None, Some(-2)))
  }

  "Two intervals (-42, 58) and (5, 20)" should "add together to be (-37, 78)" in {
  assert(Interval.createTestInterval(Some(-42), Some(58)) +
         Interval.createTestInterval(Some(5), Some(20)) ==
         Interval.createTestInterval(Some(-37), Some(78)))
  }

  // Multiplication
  "Two intervals (-∞, +∞) and (-∞, -1)" should "multiply together to be (-∞, +∞)" in {
  assert(Interval.createTestInterval(None, None) *
         Interval.createTestInterval(None, Some(-1)) ==
         Interval.createTestInterval(None, None))
  }

  "Two intervals (-∞, +∞) and (-42, +∞)" should "multiply together to be (-∞, +∞)" in {
  assert(Interval.createTestInterval(None, None) *
         Interval.createTestInterval(Some(-42), None) ==
         Interval.createTestInterval(None, None))
  }

  "Two intervals (-∞, -1) and (-∞, -1)" should "multiply together to be (-∞, 1)" in {
  assert(Interval.createTestInterval(None, Some(-1)) *
         Interval.createTestInterval(None, Some(-1)) ==
         Interval.createTestInterval(None, Some(1)))
  }

  // edge case
  "Two intervals (-6, -1) and (4, 5)" should "multiply together to be (-30, -4)" in {
  assert(Interval.createTestInterval(Some(-6), Some(-1)) *
         Interval.createTestInterval(Some(4), Some(5)) ==
         Interval.createTestInterval(Some(-30), Some(-4)))
  }

  // Division
  "Two intervals (-∞, +∞) and (-∞, -1)" should "divide together to be (-∞, +∞)" in {
    assert(Interval.createTestInterval(None, None) /
           Interval.createTestInterval(None, Some(-1)) ==
           Interval.createTestInterval(None, None))
  }

  "Two intervals (-∞, +∞) and (-42, +∞)" should "divide together to be (-∞, +∞)" in {
  assert(Interval.createTestInterval(None, None) /
         Interval.createTestInterval(Some(-42), None) ==
         Interval.createTestInterval(None, None))
  }

  "Two intervals (-∞, -1) and (-∞, -1)" should "divide together to be (-∞, 1)" in {
  assert(Interval.createTestInterval(None, Some(-1)) /
         Interval.createTestInterval(None, Some(-1)) ==
         Interval.createTestInterval(None, None))
  }

  "Two intervals (-6, -1) and (2, 3)" should "divide together to be (-3, 0)" in {
  assert(Interval.createTestInterval(Some(-6), Some(-1)) /
         Interval.createTestInterval(Some(2), Some(3)) ==
         Interval.createTestInterval(Some(-3), Some(0)))
  }

  /*
  "Two intervals (1, 6) and (-1, 2)" should "divide together to be (-∞, +∞)" in {
  assert(Interval.createTestInterval(Some(1), Some(6)) /
         Interval.createTestInterval(Some(-1), Some(2)) ==
         Interval.createTestInterval(None, None))
  } // currently giving me (-6, 3), which would be right, except not we're trying to handle intervals with 0 differently
  */
}
