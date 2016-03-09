import edu.ucsb.cs.jpf.swag.domains._
import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag._
import org.scalatest._

class RelationalNumberSpec extends FlatSpec {

  /* Internal Representation 1 and 2 */
  val numExpr1 = NumConst(1)
  val numExpr2 = NumConst(2)
  val numExpr3 = NumConst(3)
  val numExpr4 = NumConst(4)
  val numVar1 = NumVar("a")
  val numVar2 = NumVar("b")
  val numericConstraint1 = numExpr1.<(numExpr2)
  val numericConstraint2 = numVar1.<(numExpr1)
  val numericConstraint3 = numVar1.≡(numExpr1)
  val numericConstraint4 = numVar1.≠(numExpr2)
  val numericConstraint5 = ((numVar1.+(numExpr4)).-(numVar2)).>(numExpr1)
  val numericConstraint6 = numVar2.<(numExpr3)
  val conjunction0 = Conjunction(numericConstraint2)
  val conjunction1 = Conjunction(numericConstraint1, numericConstraint2)
  val conjunction2 = Conjunction(numericConstraint3)
  val conjunction3 = Conjunction(numericConstraint2, numericConstraint3)
  val conjunction4 = Conjunction(numericConstraint5, numericConstraint6)
  val conjunction5 = Conjunction(NumConst(5).<(NumVar("d")))

  val ne1 = NumConst(2).*((NumVar("a").*(NumConst(5))))
  val ne2 = NumVar("b")*(NumConst(2))
  val nc1 = (ne1.+(ne2)).>(NumConst(2))
  val ne3 = NumVar("a").+(NumVar("b"))
  val ne4 = NumVar("a").+(NumConst(3))
  val nc2 = ne3.<(ne4)
  val conjunction6 = Conjunction(nc1,nc2)

  val rnd = RelationalNumberDomain(OctagonManager())
  // a < 1 ⇒  0 < 1 - a  (make left-side negative)
  rnd.construct(conjunction0)
  println(rnd.toString())

  // a > 1 ⇒  a - 1 > 0 (make right-side negative)
  // TODO

  /*
  // 1 < 2, a < 1 ⇒  a < 1 ⇒  1 - a > 0
  rnd.construct(conjunction1)

  // a = 1 ⇒  a - 1 = 0
  rnd.construct(conjunction2)

  // a < 1, a ≠ 2
  rnd.construct(conjunction3)

  // (a + 4) - b > 1, 3 > b
  rnd.construct(conjunction4)

  // 5 < d
  rnd.construct(conjunction5)

  // 2(a * 5 + b) + (b * 2) > 2, a + b < (3 + a)
  rnd.construct(conjunction6)
  */

  /*
  // TODO eventually with string constraints involved...
  val numericConstraint7 = NumericConstraint(
   Length(
     Concat(
       StringConst("Hello"),
       Substring(
         StringVar("A"),
         NumBinopExpr(
           NumConst(3),
           NumBinop.⌜+⌝,
           NumConst(5)
         )))),
   NumComparator.<,
   LastIndexOf(
     Trim(
       StringConst("wow  ")
     ),
     CharAt(
       StringVar("B"),
       Length(
         StringConst("ye")
       ))))
  */
}
