package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._

import scala.annotation.tailrec

import scala.collection.mutable.{Map ⇒ MMap, Set ⇒ MSet, StringBuilder}

import edu.ucsb.cs.jpf.swag.helpers._

import Helpers.ι

/**
  * Helper class to extract variables from constraint trees easily.
  */
case class VariableExtractor(val strs:MSet[ID]=MSet[ID](), val nums:MSet[ID]=MSet[ID]()) {

    def addVars(c: Constraint): Unit = c match {
      case Disjunction(disjuncts) ⇒ disjuncts.map(addVars _)
      case Conjunction(conjuncts) ⇒ conjuncts.map(addVars _)
      case Not(c) ⇒ addVars(c)
      case NumericConstraint(lhs, _, rhs) ⇒
        addVars(lhs)
        addVars(rhs)
      case StringConstraint(lhs, _, rhs) ⇒
        addVars(lhs)
        addVars(rhs)
      case BoolVar(_) | True | False ⇒ ()
    }

    def addVars(e: NumExpr): Unit = e match {
      case NumVar(x) ⇒ nums += ι(x)
      case Length(e) ⇒ addVars(e)
      case IndexOf(e, _) ⇒ addVars(e)
      case LastIndexOf(e, _) ⇒ addVars(e)
      case NumUnopExpr(_, e) ⇒ addVars(e)
      case NumBinopExpr(e1, _, e2) ⇒
        addVars(e1)
        addVars(e2)
      case NumConst(_) ⇒ ()
    }

    def addVars(e: StringExpr): Unit = e match {
      case StringVar(x) ⇒ strs += ι(x)
      case Concat(e1, e2) ⇒
        addVars(e1)
        addVars(e2)
      case Substring(s, n) ⇒
        addVars(s)
        addVars(n)
      case ValueOf(n) ⇒ addVars(n)
      case Trim(e) ⇒ addVars(e)
      case ToLowerCase(e) ⇒ addVars(e)
      case ToUpperCase(e) ⇒ addVars(e)
      case Replace(e1, e2, e3) ⇒
        addVars(e1)
        addVars(e2)
        addVars(e3)
      case ReplaceAll(e1, e2, e3) ⇒
        addVars(e1)
        addVars(e2)
        addVars(e3)
      case ReplaceFirst(e1, e2, e3) ⇒
        addVars(e1)
        addVars(e2)
        addVars(e3)
      case StringConst(_) | NoStringExpr ⇒ ()
    }
}

case class NonrelationalDomain[Str <: AbstractString[Str], Num <: AbstractNumber[Num]](
  var i2s:Map[ID, Str]=Map[ID, Str](),
  var i2n:Map[ID, Num]=Map[ID, Num]())
  (
    implicit val strGen: AbstractStringFactory[Str],
    implicit val numGen: AbstractNumberFactory[Num]
  ) extends CompositeAbstractDomain[NonrelationalDomain[Str, Num]] {
  def construct(pathCondition: Constraint, stack: IndexedSeq[StackValue]) = {
    // iterative narrowing to restore precision
    val i2n = MMap[ID, Num]()
    val i2s = MMap[ID, Str]()
    // initialize all varialbes to be ⊤
    val extractor = VariableExtractor()
    extractor.addVars(pathCondition)
    for ((s, i) ← stack.zipWithIndex) s match {
      case NumValue(e) ⇒
        extractor.addVars(e)
        i2n += ι(i) → numGen.top
      case StringValue(e) ⇒
        extractor.addVars(e)
        i2s += ι(i) → strGen.top
    }
    for (n ← extractor.nums) i2n += n → numGen.top
    for (s ← extractor.strs) i2s += s → strGen.top
    // iteratively apply all constraints until reaching a fixpoint
    var stable = false
    while (!stable) {
      stable = true
      for ((s, i) ← stack.zipWithIndex) s match {
        case NumValue(e) ⇒
          val snew = compute(e)
          if (snew != i2n(StackID(i))) {
            i2n(StackID(i)) = snew
            stable = false
          }
        case StringValue(e) ⇒
          val snew = compute(e)
          if (snew != i2s(StackID(i))) {
            i2s(StackID(i)) = snew
            stable = false
          }
      }
    }
    this.i2n = i2n.toMap
    this.i2s = i2s.toMap
  }

  def transit(pathCondition: Constraint, stack: IndexedSeq[StackValue]) = {
    val newi2n = stack.zipWithIndex collect {
      case (NumValue(e), i) ⇒ StackID(i) → compute(e)
    }
    val newi2s = stack.zipWithIndex collect {
      case (StringValue(e), i) ⇒ StackID(i) → compute(e)
    }
    // TODO: use path condition
    NonrelationalDomain(newi2s.toMap, newi2n.toMap)
  }

  import NumBinop._

  def compute(e: NumExpr): Num = e match {
    case NumConst(n) ⇒ numGen.const(n.toInt)
    case NumVar(x) ⇒ i2n.getOrElse(ι(x), numGen.bottom)
    case NumBinopExpr(lhs, op, rhs) ⇒
      val l = compute(lhs)
      val r = compute(rhs)
      op match {
        case ⌜+⌝ ⇒ l + r
        case ⌜-⌝ ⇒ l - r
        case ⌜*⌝ ⇒ l * r
        case ⌜/⌝ ⇒ l / r
        case ⌜%⌝ ⇒ l % r
      }
  }

  def compute(e: StringExpr): Str = e match {
    case StringConst(s) ⇒ strGen.const(s)
    case StringVar(x) ⇒ i2s.getOrElse(ι(x), strGen.bottom)
    case Concat(lhs, rhs) ⇒ compute(lhs) concat compute(rhs)
    case NoStringExpr ⇒ strGen.bottom
  }

  def ⊔(that: NonrelationalDomain[Str, Num]) = {
    val newi2s = for (i <- i2s.keySet ++ that.i2s.keySet) yield {
      (i2s.get(i), that.i2s.get(i)) match {
        case (Some(s1), Some(s2)) => i -> (s1 ⊔ s2)
        case (None, Some(s)) => i -> s
        case (Some(s), None) => i -> s
        case (None, None) ⇒ ??? // This should not happen due to the construction in for above
      }
    }
    val newi2n = for (i <- i2n.keySet ++ that.i2n.keySet) yield {
      (i2n.get(i), that.i2n.get(i)) match {
        case (Some(s1), Some(s2)) => i -> (s1 ⊔ s2)
        case (None, Some(s)) => i -> s
        case (Some(s), None) => i -> s
        case (None, None) ⇒ ??? // This should not happen due to the construction in for above
      }
    }
    NonrelationalDomain(newi2s.toMap, newi2n.toMap)
  }

  def ⊑(that: NonrelationalDomain[Str, Num]): Boolean = {
    for ((i, s) <- that.i2s) {
      i2s.get(i) match {
        case Some(t) if !(t ⊑ s) => return false
        case None => return false
        case _ => ()
      }
    }
    for ((i, s) <- that.i2n) {
      i2n.get(i) match {
        case Some(t) if !(t ⊑ s) => return false
        case None => return false
        case _ => ()
      }
    }
    true
  }

  def ∇(that: NonrelationalDomain[Str, Num]) = {
    val newi2s = for (i <- i2s.keySet ++ that.i2s.keySet) yield {
      (i2s.get(i), that.i2s.get(i)) match {
        case (Some(s1), Some(s2)) => i -> (s1 ∇ s2)
        case (None, Some(s)) => i -> s
        case (Some(s), None) => i -> s
        case (None, None) ⇒ ??? // This should not happen due to the construction in for above
      }
    }
    val newi2n = for (i <- i2n.keySet ++ that.i2n.keySet) yield {
      (i2n.get(i), that.i2n.get(i)) match {
        case (Some(s1), Some(s2)) => i -> (s1 ∇ s2)
        case (None, Some(s)) => i -> s
        case (Some(s), None) => i -> s
        case (None, None) ⇒ ??? // This should not happen due to the construction in for above
      }
    }
    NonrelationalDomain(newi2s.toMap, newi2n.toMap)
  }

  def projectTo(id: Int): Option[Constraint] = projectTo(StackID(id))
  def projectTo(name: String): Option[Constraint] = projectTo(Name(name))
  
  def projectTo(id: ID) = {
    i2n.get(id).map(_.toConstraint(id.toString)).orElse(i2s.get(id).map(_.toConstraint(id.toString)))
  }

  def projectOut(id: Int): NonrelationalDomain[Str, Num] = projectOut(StackID(id))
  def projectOut(name: String): NonrelationalDomain[Str, Num] = projectOut(Name(name))
  
  def projectOut(id: ID): NonrelationalDomain[Str, Num] = NonrelationalDomain(i2s=i2s - id, i2n=i2n - id)
}
