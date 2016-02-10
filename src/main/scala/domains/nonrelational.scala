package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._

sealed trait ID
case class Name(s: String) extends ID {
  override def toString = s"sym_$s"
}
case class StackID(i: Int) extends ID {
  override def toString = s"stack_$i"
}

case class NonrelationalDomain[Str <: AbstractString[Str], Num <: AbstractNumber[Num]](
  i2s:Map[ID, Str]=Map[ID, Str](),
  i2n:Map[ID, Num]=Map[ID, Num]())
  (
    implicit val strGen: AbstractStringFactory[Str],
    implicit val numGen: AbstractNumberFactory[Num]
  ) extends CompositeAbstractDomain[NonrelationalDomain[Str, Num]] {
  def construct(pathCondition: Constraint, stack: IndexedSeq[StackValue]) = {
    ???
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

  def compute(e: NumExpr): Num = {
    ???
  }

  def compute(e: StringExpr): Str = e match {
    case StringConst(s) ⇒ strGen.const(s)
    case StringVar(x) ⇒
      val id = if (x.startsWith("stack_")) {
        StackID(x.substring("stack_".length).toInt)
      } else {
        Name(x)
      }
      i2s.getOrElse(id, strGen.bottom)
    case Concat(lhs, rhs) ⇒ compute(lhs) concat compute(rhs)
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
