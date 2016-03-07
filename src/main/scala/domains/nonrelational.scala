package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import scala.annotation.tailrec
import scala.language.postfixOps

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
      case NumConst(_) | NoNumExpr ⇒ ()
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
    var i2n = MMap[ID, Num]()
    var i2s = MMap[ID, Str]()
    // initialize all varialbes to be ⊤
    val extractor = VariableExtractor()
    extractor.addVars(pathCondition)
    for ((s, i) ← stack.zipWithIndex) s match {
      case NumValue(e) ⇒
        extractor.addVars(e)
        i2n += ι(i) → numGen.⊤
      case StringValue(e) ⇒
        extractor.addVars(e)
        i2s += ι(i) → strGen.⊤
    }
    for (n ← extractor.nums) i2n += n → numGen.⊤
    for (s ← extractor.strs) i2s += s → strGen.⊤
    this.i2n = i2n.toMap
    this.i2s = i2s.toMap
    // iteratively apply all constraints until reaching a fixpoint
    var stable = false
    while (!stable) {
      stable = true
      for ((s, i) ← stack.zipWithIndex) s match {
        case NumValue(e) ⇒
          val snew = compute(e)
          if (snew != this.i2n(ι(i))) {
            println(s"$snew ≠ ${i2n(ι(i))})")
            this.i2n = this.i2n + (ι(i) → snew)
            stable = false
          }
        case StringValue(e) ⇒
          val snew = compute(e)
          if (snew != this.i2s(ι(i))) {
            println(s"$snew ≠ ${i2s(ι(i))})")
            this.i2s = this.i2s + (ι(i) → snew)
            stable = false
          }
      }
      // process path condition
      val (newi2n, newi2s) = narrowOnConstraint(this.i2n, this.i2s)(pathCondition)
      if ((newi2n → newi2s) != (this.i2n → this.i2s)) { // TODO: optimization: use a flag
        stable = false
      }

      println(s"old:${(this.i2n, this.i2s)}\n${Console.GREEN}new:${(newi2n, newi2s)}${Console.RESET}")

      // TODO: optimize this replacement, maybe switch to MMap
      this.i2n = newi2n
      this.i2s = newi2s
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

  def narrowOnConstraint(i2n: Map[ID, Num], i2s: Map[ID, Str])(c: Constraint): (Map[ID, Num], Map[ID, Str]) = c match {
    case Disjunction(cs) ⇒
      cs map narrowOnConstraint(i2n, i2s) reduce { (t1, t2) ⇒
        val i2n = for (i ← t1._1.keySet ++ t2._1.keySet) yield
          (t1._1.get(i), t2._1.get(i)) match {
            case (Some(s1), Some(s2)) => i -> (s1 ⊔ s2)
            case (None, Some(s)) => i -> s
            case (Some(s), None) => i -> s
            case (None, None) ⇒ ??? // This should not happen due to the construction in for above
          }
        val i2s = for (i ← t1._2.keySet ++ t2._2.keySet) yield
          (t1._2.get(i), t2._2.get(i)) match {
            case (Some(s1), Some(s2)) => i -> (s1 ⊔ s2)
            case (None, Some(s)) => i -> s
            case (Some(s), None) => i -> s
            case (None, None) ⇒ ??? // This should not happen due to the construction in for above
          }
        (i2n.toMap, i2s.toMap)
      }
    case Conjunction(cs) ⇒
      cs.foldLeft((i2n, i2s))((t, c) ⇒ narrowOnConstraint(t._1, t._2)(c))
    case c@NumericConstraint(NumVar(x), op, e) ⇒
      val newi2n = i2n + (ι(x) → i2n(ι(x)).addConstraint(op, compute(e)))
      //println(Console.YELLOW + s"Simple constraint ${Console.CYAN}$c${Console.RESET} rhs: ${Console.GREEN}${compute(e)}" + Console.RESET)
      (newi2n, i2s)
      // TODO: add more clever stuff on numeric constraints
    case c@NumericConstraint(e1, op, e2) ⇒
      // heuristics to extract different types of inequalities on single vars
      enumerateVars(c).foldLeft((i2n, i2s)) { (maps, c) ⇒
        narrowOnConstraint(maps._1, maps._2)(c)
      }
    case StringConstraint(StringVar(x), op, e) ⇒
      op match {
        case StringComparator.⌜==⌝ | StringComparator.eq ⇒
          (i2n, i2s + (ι(x) → (compute(e))))
        case _ ⇒ (i2n, i2s) // TODO: consider other cases
      }
    case _ ⇒ (i2n, i2s) // TODO: add other constraints
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
    case NumVar(x) ⇒
      i2n.getOrElse(ι(x), numGen.⊤)
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
    case IndexOf(s, c) ⇒ compute(compute(s).indexOf(c))
    case LastIndexOf(s, c) ⇒ compute(compute(s).lastIndexOf(c))
    case Length(s) ⇒
      // TODO: gensym for length variable
      NonrelationalDomain[Str, Num]().construct(compute(s).length("length"), IndexedSeq()).i2n(ι("length"))
    case NoNumExpr ⇒ numGen.⊥
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

  override def toString = {
    val sb = new StringBuilder("NonrelationalDomain(\n")
    for ((x, s) ← i2s)
      sb ++= s"\t$x → $s\n"
    for ((x, n) ← i2n)
      sb ++= s"\t$x → $n\n"
    sb ++= ")"
    sb.toString
  }

  def toConstraint = Conjunction((i2n ++ i2s) map {
    case (id, v) ⇒ v.toConstraint(id.toString)
  } toSet)
}
