package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag.helpers._
import edu.ucsb.cs.jpf.swag.constraints.NumComparator.NumComparator
import scala.annotation.tailrec
import scala.collection.mutable.{Map ⇒ MMap, Set ⇒ MSet, StringBuilder}

import apron._

import Helpers.ι

// problem is getting from our constraints to
// apron and back

// is this just going to store information about
// one single abstract element...
// or multiple abstract elements (via an Abstract1 + env?)

case class RelationalNumberDomain
(man: Manager)
extends RelationalNumber[RelationalNumberDomain] {
  var absNumber: Abstract1 = new Abstract1(man, new Environment())
  val spfVar2ApronVar: MMap[NumVar, ApronVar] = MMap()

  def ⊔(that: RelationalNumberDomain): RelationalNumberDomain = {
    ???
  }

  override def ∇(that: RelationalNumberDomain) = this ⊔ that

  def ⊑(that: RelationalNumberDomain): Boolean = {
    ???
  }

  case class ApronVar(name: String, spfVar: NumVar) { }

  object ApronVar {
    private var counter = 0;
    def apply(v: NumVar): ApronVar = { counter += 1; ApronVar("V" + counter.toString, v) }
  }

  def getApronVar(v: NumVar): ApronVar = {
    if (!spfVar2ApronVar.contains(v))
      spfVar2ApronVar += (v →  ApronVar(v))
    spfVar2ApronVar(v)
  }


  case class CoefficientMap(v2c: Map[NumVar, Int] = Map(), const: Int = 0) {
    def combine(other: CoefficientMap, f: (Int, Int) ⇒  Int): CoefficientMap = {
      val ov2c = other.v2c
      val oconst = other.const
      val aKeys = v2c.keys
      val bKeys = ov2c.keys
      val aIntersectB = aKeys.toSet.intersect(bKeys.toSet)
      val justAKeys = aKeys.toSet -- aIntersectB
      val justBKeys = bKeys.toSet -- aIntersectB

      CoefficientMap(
        aIntersectB.map(k ⇒ k → (f(v2c(k),ov2c(k)))).toMap ++
          justAKeys.map(k ⇒ k → v2c(k)).toMap ++
          justBKeys.map(k ⇒ k → ov2c(k))toMap,
        f(const, oconst)
      )
    }

    def applyConst(c: Int, f: (Int, Int) ⇒  Int) =
      CoefficientMap(v2c.keys.map(k ⇒  k →  f(c,v2c(k))).toMap, f(const,c))

    override def toString(): String = {
      s"γ -> $const\n" + v2c.map(p ⇒  s"${p._1.name}(${getApronVar(p._1).name})" → p._2).mkString("\n")
    }
  }

  def formConstraint(cMap1: CoefficientMap, cMap2: CoefficientMap, comparator: Int): Lincons1 = {
    val cmap = cMap1.combine(cMap2, _+_)
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
  def parseNumExpr(expr: NumExpr): CoefficientMap = expr match {
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
          /* same as above */
          case (m1@CoefficientMap(mp, c), m2) if mp.isEmpty ⇒ m2.applyConst(c, _/_)
          case (m1, m2@CoefficientMap(mp, c)) if mp.isEmpty ⇒ m1.applyConst(c, _/_)
          case _ ⇒ sys.error("non-linear constraint")
        }
        case NumBinop.⌜%⌝ ⇒  ???
      }
    }
    case NoNumExpr ⇒ CoefficientMap()
  }

  def construct(constraints: Conjunction) = {
    /* Remove any previous */
    absNumber = new Abstract1(man, new Environment())
    spfVar2ApronVar.clear
    constraints.conjuncts.foreach(addConstraint(_))
    this
  }

  // TODO return set of vars to values...?
  def getVars(): Set[NumVar] = ???

  def projectTo(id: ID): Option[Constraint]  = {
    ???
  }

  def projectOut(id: ID): RelationalNumberDomain = {
    ???
  }

  // add extra constraint
  def addConstraint(c: Constraint): RelationalNumberDomain = c match {
    case Not(c: Constraint) ⇒  ??? // TODO ...
    case Disjunction(disjuncts) ⇒  ??? // TODO ...
    case Conjunction(conjuncts) ⇒  sys.error("should be given in CNF")
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
      /* TODO: Review this */
      absNumber = absNumber.unifyCopy(man, absN)
      this
    }
  }

  // Constraint representation of this abstract domain
  def toConstraint: Constraint = ???

  def toConstraint(v: String): Constraint = ???

  override def toString(): String = absNumber.toString()
}

object OctagonManager {
  def apply(): Octagon = new Octagon()
}

object Debug {
  var debug = true

  def print(s: String) = if (debug) println(s)
}
