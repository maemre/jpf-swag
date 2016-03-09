package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag.helpers._
import scala.annotation.tailrec
import scala.collection.mutable.{Map ⇒ MMap, Set ⇒ MSet, StringBuilder}
import scala.language.postfixOps

import Helpers.ι

/**
  * Wrapper class to integrate nonrelational string domains into semirelational
  * string domains.
  */
case class SemirelationalStringWrapper[Str <: AbstractString[Str]]
  (var i2s: Map[ID, Str] = Map[ID, Str]())
  (implicit val strGen: AbstractStringFactory[Str])
    extends RelationalString[SemirelationalStringWrapper[Str]] {

  def construct(constraints: Conjunction) = {
    // iterative narrowing to restore precision

    // initialize all varialbes to be ⊤
    val extractor = VariableExtractor()
    extractor.addVars(constraints)
    for (s ← extractor.strs) i2s = i2s + s → strGen.⊤
    // iteratively apply all constraints until reaching a fixpoint
    var stable = false
    while (!stable) {
      stable = true
      // process condition
      val newi2s = constraints.conjuncts.foldLeft(i2s)(narrowOnConstraint)
      if (newi2s != i2s) { // TODO: optimization: use a flag
        stable = false
      }

      println(s"old:${i2s}\n${Console.GREEN}new:${newi2s}${Console.RESET}")

      // TODO: optimize this replacement, maybe switch to MMap
      i2s = newi2s
    }

    this
  }

  def enumerateVars(c: NumericConstraint): Seq[NumericConstraint] = c match {
    case NumericConstraint(v:NumVar, op, e) ⇒ Seq(c)
    case NumericConstraint(NumBinopExpr(e1, NumBinop.⌜+⌝, e2), op, rhs) ⇒
      val e = enumerateVars(NumericConstraint(e1, op, rhs - e2)) ++
      enumerateVars(NumericConstraint(e2, op, rhs - e1))
      if (e.nonEmpty) {
        //println(s"${Console.RED} enumerated vars: ${Console.RESET} $e FROM $c")
      }
      e
    case _ ⇒ Seq()
  }

  def narrowOnConstraint(i2s: Map[ID, Str], c: Constraint): Map[ID, Str] = c match {
    case Disjunction(cs) ⇒
      cs map { narrowOnConstraint(i2s, _) } reduce { (t1, t2) ⇒
        val i2s = for (i ← t1.keySet ++ t2.keySet) yield
          (t1.get(i), t2.get(i)) match {
            case (Some(s1), Some(s2)) => i -> (s1 ⊔ s2)
            case (None, Some(s)) => i -> s
            case (Some(s), None) => i -> s
            case (None, None) ⇒ ??? // This should not happen due to the construction in for above
          }
        i2s.toMap
      }
    case Conjunction(cs) ⇒
      cs.foldLeft(i2s)(narrowOnConstraint)
    case StringConstraint(StringVar(x), op, e) ⇒
      op match {
        case StringComparator.⌜==⌝ | StringComparator.eq ⇒
          i2s + (ι(x) → (compute(e)))
        case _ ⇒ i2s // TODO: consider other cases
      }
    case _ ⇒ i2s // TODO: add other constraints
  }

  def compute(e: StringExpr): Str = e match {
    case StringConst(s) ⇒ strGen.const(s)
    case StringVar(x) ⇒ i2s.getOrElse(ι(x), strGen.⊥)
    case Concat(lhs, rhs) ⇒ compute(lhs) concat compute(rhs)
    case NoStringExpr ⇒ strGen.⊥
    case Replace(s, m, r) ⇒ compute(s).replace(compute(m), compute(r))
    case ReplaceAll(s, m, r) ⇒ compute(s).replaceAll(compute(m), compute(r))
    case ReplaceFirst(s, m, r) ⇒ compute(s).replaceFirst(compute(m), compute(r))
    case Substring(s, i) ⇒ compute(s).substring(i)
    case ToLowerCase(s) ⇒ compute(s).toLowerCase
    case ToUpperCase(s) ⇒ compute(s).toUpperCase
    case Trim(s) ⇒ compute(s).trim
    case ValueOf(n) ⇒ strGen.valueOf(n)
  }

  def ⊔(that: SemirelationalStringWrapper[Str]) = {
    val newi2s = for (i <- i2s.keySet ++ that.i2s.keySet) yield {
      (i2s.get(i), that.i2s.get(i)) match {
        case (Some(s1), Some(s2)) => i -> (s1 ⊔ s2)
        case (None, Some(s)) => i -> s
        case (Some(s), None) => i -> s
        case (None, None) ⇒ ??? // This should not happen due to the construction in for above
      }
    }
    copy(newi2s.toMap)
  }

  def ⊑(that: SemirelationalStringWrapper[Str]): Boolean = {
    for ((i, s) <- that.i2s) {
      i2s.get(i) match {
        case Some(t) if !(t ⊑ s) => return false
        case None => return false
        case _ => ()
      }
    }
    true
  }

  override def ∇(that: SemirelationalStringWrapper[Str]) = {
    val newi2s = for (i <- i2s.keySet ++ that.i2s.keySet) yield {
      (i2s.get(i), that.i2s.get(i)) match {
        case (Some(s1), Some(s2)) => i -> (s1 ∇ s2)
        case (None, Some(s)) => i -> s
        case (Some(s), None) => i -> s
        case (None, None) ⇒ ??? // This should not happen due to the construction in for above
      }
    }
    copy(newi2s.toMap)
  }

  def projectTo(id: Int): Option[Constraint] = projectTo(StackID(id))
  def projectTo(name: String): Option[Constraint] = projectTo(Name(name))
  
  def projectTo(id: ID) = i2s.get(id).map(_.toConstraint(id.toString))

  def projectOut(id: Int): SemirelationalStringWrapper[Str] = projectOut(StackID(id))
  def projectOut(name: String): SemirelationalStringWrapper[Str] = projectOut(Name(name))

  def projectOut(id: ID): SemirelationalStringWrapper[Str] = SemirelationalStringWrapper(i2s=i2s - id)

  override def toString = {
    val sb = new StringBuilder("SemirelationalStringWrapper(\n")
    for ((x, s) ← i2s)
      sb ++= s"\t$x → $s\n"
    sb ++= ")"
    sb.toString
  }

  def lengthConstraint(id: ID, l: String): Option[Constraint] = i2s.get(id).map(_.length(l))

  def toConstraint = Conjunction(i2s map {
    case (id, v) ⇒ v.toConstraint(id.toString)
  } toSet)

  def getVars = i2s.keySet
}
