package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag.helpers._
import edu.ucsb.cs.jpf.swag.constraints.NumComparator.NumComparator
import scala.annotation.tailrec
import scala.collection.mutable.{Map ⇒ MMap, Set ⇒ MSet, StringBuilder}

import apron._

import Helpers.ι

case class RelationalNumberDomain
(man: Manager)
extends RelationalNumber[RelationalNumberDomain] {
  var absNumber: Abstract1 = new Abstract1(man, new Environment())
  val irVar2ApronVar: MMap[NumVar, ApronVar] = MMap()

  // Note: this returns a copy, unlike construct(). This can easily be changed
  // TODO also, what if managers between both are different?
  def ⊔(that: RelationalNumberDomain): RelationalNumberDomain = {
    val rnd = RelationalNumberDomain(man)
    
    // !!!! This will need to be fixed if using unique Apron vars (see ApronVar object comments)
    irVar2ApronVar.foreach(p ⇒ rnd.irVar2ApronVar += p)
    that.irVar2ApronVar.foreach(p ⇒ rnd.irVar2ApronVar += p)

    Debug.print(s"Mapping after joining: ${rnd.showIR2ApronMap}")
    val newEnv = absNumber.getEnvironment().lce(that.absNumber.getEnvironment)
    val oAbsNum = that.absNumber.changeEnvironmentCopy(that.man, newEnv, true)
    rnd.absNumber = absNumber.changeEnvironmentCopy(man, newEnv, true)
    rnd.absNumber.join(man, oAbsNum)
    rnd
  }

  override def ∇(that: RelationalNumberDomain) = this ⊔ that

  def ⊑(that: RelationalNumberDomain): Boolean = {
    absNumber.isIncluded(man, that.absNumber)
  }

  def getApronVar(v: NumVar): ApronVar = {
    if (!irVar2ApronVar.contains(v))
      irVar2ApronVar += (v →  ApronVar(v))
    irVar2ApronVar(v)
  }

  case class CoefficientMap(v2c: Map[NumVar, Int] = Map(), const: Int = 0) {
    def combine(other: CoefficientMap, f: (Int, Int) ⇒  Int): CoefficientMap = {
      val ov2c = other.v2c
      val oconst = other.const

      CoefficientMap(
        (v2c.keys ++ ov2c.keys).map(k ⇒  (v2c.get(k), ov2c.get(k)) match {
          case (Some(i), Some(j)) ⇒  k → f(i,j)
          case (Some(i), None) ⇒  k → f(i, 0)
          case (None, Some(i)) ⇒  k → f(0, i)
          case (None, None) ⇒ sys.error("impossible condition in combine")
        }).toMap,
        f(const, oconst)
      )
    }

    def applyConst(c: Int, f: (Int, Int) ⇒  Int) =
      CoefficientMap(v2c.keys.map(k ⇒  k →  f(c,v2c(k))).toMap, f(const,c))

    override def toString(): String = {
      s"γ(cc) -> $const\n" +
        (v2c.map(p ⇒  s"${p._1.name}(${getApronVar(p._1).name})" → p._2).mkString("\n")).trim
    }
  }

  def formConstraint(cMap1: CoefficientMap, cMap2: CoefficientMap, comparator: Int): Lincons1 = {
    val cmap = cMap1.combine(cMap2, _+_)
    Debug.print(s"Combined coefficients:\n${cmap}")
    val terms = cmap.v2c.map(
      { case (nVar, coeff) ⇒  new Linterm1(getApronVar(nVar).name, new MpqScalar(coeff)) }
    ).toArray
    val vars = terms.map(_.getVariable()).toArray
    val env = new Environment(vars, Array())
    val constCoeff = new MpqScalar(cmap.const)
    val linexp = new Linexpr1(env, terms, constCoeff)
    val cons = new Lincons1(comparator, linexp)
    cons
  }

  /* Side-effect of adding to map */
  def parseNumExpr(expr: NumExpr): CoefficientMap = {
    Debug.print(s"Parsing: $expr")
    expr match {
      case Length(strExpr) ⇒  CoefficientMap()
      case IndexOf(strExpr, chExpr) ⇒  CoefficientMap()
      case LastIndexOf(strExpr, chExpr) ⇒  CoefficientMap()
      case v:NumVar ⇒ CoefficientMap(Map(v → 1))
      case NumConst(n) ⇒ CoefficientMap(const = n.toInt) // TODO I've going from Long to Int here; is that going to hurt anything?
      case NumBinopExpr(lhs, op, rhs) ⇒  {
        val lMap = parseNumExpr(lhs)
        val rMap = parseNumExpr(rhs)
        op match {
          case NumBinop.⌜+⌝ ⇒ lMap.combine(rMap, _+_)
          case NumBinop.⌜-⌝ ⇒ lMap.combine(rMap, _-_)
          case NumBinop.⌜*⌝ ⇒  (lMap, rMap) match {
            /* one of the two maps must just contain a constant */
            case (m1@CoefficientMap(mp, c), m2) if mp.isEmpty ⇒ m2.applyConst(c, _*_)
            case (m1, m2@CoefficientMap(mp, c)) if mp.isEmpty ⇒ m1.applyConst(c, _*_)
            case _ ⇒ sys.error("non-linear constraint")
          }
          case NumBinop.⌜/⌝ ⇒  (lMap, rMap) match {
            // TODO fix...since we're dealing with linear constraints,
            //  division won't really work, we need to multiply everything
            //  else by this divisor, i.e. a/3 > 8 ⇒ a > 24
            case (m1@CoefficientMap(mp, c), m2) if mp.isEmpty ⇒ m2.applyConst(c, _/_)
            case (m1, m2@CoefficientMap(mp, c)) if mp.isEmpty ⇒ m1.applyConst(c, _/_)
            case _ ⇒ sys.error("non-linear constraint")
          }
          case NumBinop.⌜%⌝ ⇒  ???
        }
      }
      case NoNumExpr ⇒ CoefficientMap()
    }
  }

  // Side effect: changes this RND, does NOT return a copy (I thought this made sense in the situation)
  def construct(constraints: Conjunction) = {
    /* Remove any previous */
    absNumber = new Abstract1(man, new Environment())
    irVar2ApronVar.clear
    constraints.conjuncts.foreach(addConstraint(_))
    this
  }

  def showIR2ApronMap =
    irVar2ApronVar.map({ case (k,v) ⇒  k →  v.name}).mkString(", ")

  def projectTo(id: ID): Option[Constraint]  = {
    ???
  }

  /* Side-effect: changes this */
  def projectOut(id: ID): RelationalNumberDomain = {
    absNumber.forget(man, Array(id.toString()), true)
    this
  }

  // add extra constraint
  def addConstraint(c: Constraint): RelationalNumberDomain = c match {
    case Not(c: Constraint) ⇒  ??? // TODO ...
    case Disjunction(disjuncts) ⇒  ??? // TODO ...
    case Conjunction(conjuncts) ⇒
      conjuncts.foldLeft(this)((dom, c) ⇒ dom.addConstraint(c))
    case True ⇒  this/* ignore for now */
    case False ⇒  this /* ignore for now */
    case s:StringConstraint ⇒  this /* later */
    case n@NumericConstraint(lhs: NumExpr, op: NumComparator, rhs: NumExpr) ⇒ {
      Debug.print(s"Encountered numeric constraint: $n")
      /* Must be of the form:
          expr comparator 0
         So
          expr1 comparator expr2
         must be turned into
          expr1 + (-expr2) comparator 0

         Since Apron doesn't have the < or ≤ comparators, must
         move the stuff of the left-hand-side of < or ≤ to the right.

         E.g.:
          a < 1 ⇒ 0 < 1 - a (make left-side negative)
          a > 1 ⇒ a - 1 > 0 (make right-side negative)
      */
      val expr1Map =
        if (op == NumComparator.< || op == NumComparator.≤) parseNumExpr(lhs.unary_-)
        else parseNumExpr(rhs.unary_-)
      val expr2Map =
        if (op == NumComparator.< || op == NumComparator.≤) parseNumExpr(rhs)
        else parseNumExpr(lhs)

      Debug.print(s"Expr1 coefficients:\n$expr1Map")
      Debug.print(s"Expr2 coefficients:\n$expr2Map")

      val comp = op match {
        case NumComparator.≡ ⇒ Lincons1.EQ
        case NumComparator.≠ ⇒ Lincons1.DISEQ
        case NumComparator.> ⇒ Lincons1.SUP
        case NumComparator.≥ ⇒ Lincons1.SUPEQ
        case NumComparator.< ⇒ Lincons1.SUP /* negation taken care of above */
        case NumComparator.≤ ⇒ Lincons1.SUPEQ /* ditto */
      }
      val lincons = formConstraint(expr1Map, expr2Map, comp)
      val absN = new Abstract1(man, Array(lincons)) 
      absNumber = absNumber.unifyCopy(man, absN)
      this
    }
  }

  // Constraint representation of this abstract domain
  def toConstraint: Constraint = {

    Debug.print("IR-to-Apron variable map:")
    Debug.print(s"${irVar2ApronVar.toString}")

    def getCoeffValue(c: Coeff): Long = c match {
      case s:Scalar ⇒ {
        // Could be MpqScalar, MpfrScalar, or DoubleScalar
        var arr = Array[Double](0)
        s.toDouble(arr, 0) // investigate how 0 is used for rounding...
        arr(0).toLong
      }
      case i:apron.Interval ⇒ sys.error("not handling intervals right now")
    }

    val conjuncts: Set[Constraint] = absNumber.toLincons(man).map(cons ⇒  {
      val op = cons.getKind() match {
        case Lincons1.EQ    ⇒  NumComparator.≡
        case Lincons1.DISEQ ⇒  NumComparator.≠
        case Lincons1.SUP   ⇒  NumComparator.>
        case Lincons1.SUPEQ ⇒  NumComparator.≥
      }
      val termsSep = cons.getLinterms.map(term ⇒  {
        irVar2ApronVar.find(p ⇒ p._2.name == term.getVariable()) match {
          case Some(p) ⇒ 
            NumBinopExpr(p._1, NumBinop.⌜*⌝, NumConst(getCoeffValue(term.getCoefficient())))
          case None ⇒ 
            sys.error(s"error converting from Apron to IR (couldn't find ${term.getVariable()})")
        }
      })
      val constCoeffTerm = NumConst(getCoeffValue(cons.getCst))
      val termsAdded = termsSep.foldLeft(constCoeffTerm: NumExpr)((lhs, term) ⇒ 
        NumBinopExpr(lhs, NumBinop.⌜+⌝, term))
      NumericConstraint(termsAdded, op, NumConst(0))
    }).toSet
    Conjunction(conjuncts)
  }

  def toConstraint(v: String): Constraint = ???

  override def toString(): String = absNumber.toString()
}

// Even though the map tracks what IR var goes to which ApronVar,
// the information might as well be stored along with the ApronVar too
case class ApronVar(name: String, irVar: NumVar) { }

object ApronVar {
  private var counter = 0;

  /* Note the 2 different options below: */

  /* Uncomment the following to use the IR's variable names directly in Apron */
  def apply(v: NumVar): ApronVar = ApronVar(v.name, v)

  /* Uncomment the following to use fresh, unique-to-Apron variable names. This isn't working with join, so using the above */
  //def apply(v: NumVar): ApronVar = { counter += 1; ApronVar("V" + counter.toString, v) }
}

object BoxManager {
  def apply(): Box = new Box()
}
object OctagonManager {
  def apply(): Octagon = new Octagon()
}
object PolyhedraManager {
  def apply(): Polka = new Polka(false)
}
/*object PplPoly {
  def apply(): PplPoly = new PplPoly(false)
}
 */
object Debug {
  var debug = false

  def print(s: String) = if (debug) println(s)
}
