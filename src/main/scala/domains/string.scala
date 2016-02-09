package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import scala.annotation.tailrec

sealed trait Prefix extends AbstractString[Prefix] {
  @tailrec
  private def lcprec(w: String, v: String, i: Int, l: Int): Int = if (i == l || w(i) != v(i)) {
    i
  } else {
    lcprec(w, v, i+1, l)
  }
  
  /**
   * Computes longest common prefix of two strings
   */
  def lcp(w: String, v: String) = {
    val len = lcprec(w, v, 0, w.length min v.length)
    w.substring(0, len)
  }
  
  @tailrec
  final def ⊔(s: Prefix): Prefix = (this, s) match {
    case (NoString, _) => s
    case (_, NoString) => this
    case (PrefixS(w), ConstS(v)) if v.startsWith(w) => this
    case (PrefixS(w), ConstS(v)) => PrefixS(lcp(w, v))
    case (ConstS(_), PrefixS(_)) => s ⊔ this
    case (ConstS(w), ConstS(v)) =>
      if (w == v) {
        this
      } else {
        PrefixS(lcp(w, v))
      }
  }
  
  def ⊑(s: Prefix): Boolean
}

case object NoString extends Prefix {
  def concat(s: Prefix) = this
  override def ⊑(s: Prefix): Boolean = true
  def toConstraint(v: String) = False
}

case class ConstS(s: String) extends Prefix {
  def concat(that: Prefix) = that match {
    case NoString => NoString
    case ConstS(t) => ConstS(s + t)
    case PrefixS(t) => PrefixS(s + t)
  }
  
  def ⊑(that: Prefix): Boolean = that match {
    case ConstS(t) => s == t
    case PrefixS(t) => s.startsWith(t)
    case _ ⇒ false
  }

  def toConstraint(v: String) = StringConstraint(StringVar(v), StringComparator.eq, StringConst(s))
}
case class PrefixS(s: String) extends Prefix {
  def concat(that: Prefix) = that match {
    case NoString => NoString
    case _ => this
  }
  
  def ⊑(that: Prefix): Boolean = that match {
    case PrefixS(t) => s.startsWith(t)
    case _ ⇒ false
  }

  def toConstraint(v: String) = StringConstraint(StringVar(v), StringComparator.startsWith, StringConst(s))
}
