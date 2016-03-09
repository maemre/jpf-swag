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
  val numericConstraint8 = NumVar("a").<(NumConst(-1))
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
  val conjunction8 = Conjunction(nc1)
  val conjunction6 = Conjunction(nc1,nc2)
  val conjunction7 = Conjunction(NumVar("a").>(NumConst(1)))
  val conjunction9 = Conjunction(numericConstraint8)

  val rnd = RelationalNumberDomain(PolyhedraManager())
  
  // The following don't actually check the results of
  // tests, but are helpful if visually insepcted.

  // a > 1 ⇒ 
  // a - 1 > 0 (make right-side negative)
  // Apron will produce: a - 2 ≥ 0
  println("\nStarting Test1")
  rnd.construct(conjunction7)
  println(s"In IR      : ${conjunction7.toString}")
  println(s"In Apron   : ${rnd.toString}")
  println(s"Back to IR : ${rnd.toConstraint.toString}")
  println(s"IR-to-Apron Mapping: ${rnd.showIR2ApronMap}")

  // 1 < 2, a < 1 ⇒ 
  // a < 1 ⇒ 
  // 1 - a > 0
  // Apron will produce: -a ≥ 0
  println("\nStarting Test2")
  rnd.construct(conjunction1)
  println(s"In IR      : ${conjunction1.toString}")
  println(s"In Apron   : ${rnd.toString}")
  println(s"Back to IR : ${rnd.toConstraint.toString}")
  println(s"IR-to-Apron Mapping: ${rnd.showIR2ApronMap}")

  // a = 1 ⇒ 
  // a - 1 = 0
  // Apron will produce: a - 1 = 0
  println("\nStarting Test3")
  rnd.construct(conjunction2)
  println(s"In IR      : ${conjunction2.toString}")
  println(s"In Apron   : ${rnd.toString}")
  println(s"Back to IR : ${rnd.toConstraint.toString}")
  println(s"IR-to-Apron Mapping: ${rnd.showIR2ApronMap}")

  // a < 1, a = 1
  // Apron will produce: <empty> (unsat)
  println("\nStarting Test4")
  rnd.construct(conjunction3)
  println(s"In IR      : ${conjunction3.toString}")
  println(s"In Apron   : ${rnd.toString}")
  println(s"Back to IR : ${rnd.toConstraint.toString}")
  println(s"IR-to-Apron Mapping: ${rnd.showIR2ApronMap}")

  // (a + 4) - b > 1, 3 > b
  // Apron will produce: -b + 2 ≥ 0, a - b + 2 ≥ 0 (need to check if correct)
  println("\nStarting Test5")
  rnd.construct(conjunction4)
  println(s"In IR      : ${conjunction4.toString}")
  println(s"In Apron   : ${rnd.toString}")
  println(s"Back to IR : ${rnd.toConstraint.toString}")
  println(s"IR-to-Apron Mapping: ${rnd.showIR2ApronMap}")

  // 5 < d
  // Apron will produce: d - 6 ≥ 0
  println("\nStarting Test6")
  rnd.construct(conjunction5)
  println(s"In IR      : ${conjunction5.toString}")
  println(s"In Apron   : ${rnd.toString}")
  println(s"Back to IR : ${rnd.toConstraint.toString}")
  println(s"IR-to-Apron Mapping: ${rnd.showIR2ApronMap}")

  // 2(a * 5) + (b * 2) > 2
  // ⇒  10a + 2b > 2
  // ⇒  10a + 2b - 2 > 0
  // Apron produces: 5a + b - 2 ≥ 0 (which is the same)
  println("\nStarting Test7")
  rnd.construct(conjunction8)
  println(s"In IR      : ${conjunction8.toString}")
  println(s"In Apron   : ${rnd.toString}")
  println(s"Back to IR : ${rnd.toConstraint.toString}")
  println(s"IR-to-Apron Mapping: ${rnd.showIR2ApronMap}")

  // 2(a * 5) + (b * 2) > 2, a + b < (3 + a)
  // ⇒  10a + 2b > 2, b < 3
  // ⇒  5a + b > 1, 0 < 3 - b
  // ⇒  5a + b - 1 > 0, 0 < 3 - b
  // ⇒  5a + b - 2 ≥ 0, 0 ≤ 2 - b
  // Apron produces: 5a + b - 2 ≥ 0, -b + 2 ≥ 0 (i.e. the same)
  println("\nStarting Test8")
  rnd.construct(conjunction6)
  println(s"In IR      : ${conjunction6.toString}")
  println(s"In Apron   : ${rnd.toString}")
  println(s"Back to IR : ${rnd.toConstraint.toString}")
  println(s"IR-to-Apron Mapping: ${rnd.showIR2ApronMap}")

  println("\nStarting Test9 (joining)")
  val rnd1 = RelationalNumberDomain(PolyhedraManager())
  val rnd2 = RelationalNumberDomain(PolyhedraManager())
  rnd1.construct(conjunction1) // -a ≥ 0     (i.e. a < 1)
  rnd2.construct(conjunction9) // -a - 2 ≥ 0 (i.e. a < -1)
  val rnd3 = rnd1 ⊔ rnd2
  // Apron produces -a ≥ 0 (i.e. a < 1), TODO is this what we want?
  println(s"Test9 conjunct 1: ${conjunction1.toString}")
  println(s"Test9 conjunct 2: ${conjunction9.toString}")
  println(s"Test9 (joining) AN1: ${rnd1.toString}")
  println(s"Test9 (joining) AN2: ${rnd2.toString}")
  println(s"Test9 (result of join): ${rnd3.toString}")
  println(s"Back to IR: ${rnd3.toConstraint.toString}")
  println(s"IR-to-Apron Mapping: ${rnd3.showIR2ApronMap}")

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
