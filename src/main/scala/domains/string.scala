package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import scala.annotation.tailrec

object ImplicitShim {
  implicit object PrefixFactory extends AbstractStringFactory[Prefix] {
    val top = PrefixS("")
    val bottom = NoString
    def const(s: String) = ConstS(s)
    def valueOf(n: NumExpr) = n match {
      case NumConst(n) ⇒ const(n.toString)
      case _ ⇒ top
    }
  }
}

import ImplicitShim._

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

  def indexOf(c: CharExpr): NumExpr = NoNumExpr
  def lastIndexOf(c: CharExpr): NumExpr = NoNumExpr
  def length(v: String) = NumericConstraint(NumVar(v), NumComparator.≡, NoNumExpr)
  def replace(m: Prefix,r: Prefix) = NoString
  def replaceAll(m: Prefix,r: Prefix) = NoString
  def replaceFirst(m: Prefix,r: Prefix) = NoString
  def substring(i: NumExpr) = NoString
  def toLowerCase = NoString
  def toUpperCase = NoString
  def trim = NoString
}

case class ConstS(s: String) extends Prefix {
  def concat(that: Prefix) = that match {
    case NoString => NoString
    case ConstS(t) => ConstS(s + t)
    case PrefixS(t) => PrefixS(s + t)
  }
  
  def indexOf(c: CharExpr): NumExpr = c match {
    case CharConst(c) ⇒ NumConst(s indexOf c)
    case _ ⇒ ???
  }
  def lastIndexOf(c: CharExpr): NumExpr = c match {
    case CharConst(c) ⇒ NumConst(s lastIndexOf c)
    case _ ⇒ ???
  }
  def length(v: String) = NumericConstraint(NumVar(v), NumComparator.≡, NumConst(s.length))
  def replace(m: Prefix, r: Prefix) = ???
  def replaceAll(m: Prefix, r: Prefix) = ???
  def replaceFirst(m: Prefix, r: Prefix) = ???
  def substring(i: NumExpr) = i match {
    case NumConst(i) ⇒ if (i <= s.length) ConstS(s.substring(i.toInt)) else NoString
    case _ ⇒ PrefixFactory.top
  }

  def toLowerCase = copy(s.toLowerCase)
  def toUpperCase = copy(s.toUpperCase)
  def trim = copy(s.trim)

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
  def indexOf(c: CharExpr): NumExpr = c match {
    case CharConst(c) ⇒
      val constIdx = s indexOf c
      if (constIdx >= 0)
        NumConst(constIdx)
      else
        ???
    case _ ⇒ ???
  }
  def lastIndexOf(c: CharExpr): NumExpr = ???
  def length(v: String) = NumericConstraint(NumVar(v), NumComparator.≥, NumConst(s.length))
  def replace(m: Prefix, r: Prefix) = ???
  def replaceAll(m: Prefix, r: Prefix) = ???
  def replaceFirst(m: Prefix, r: Prefix) = ???

  def substring(i: NumExpr) = i match {
    case NumConst(i) ⇒ if (i <= s.length) PrefixS(s.substring(i.toInt)) else PrefixS("")
    case _ ⇒ PrefixFactory.top
  }

  def toLowerCase = copy(s.toLowerCase)
  def toUpperCase = copy(s.toUpperCase)
  def trim = copy(s.trim) // trim works instead of trimLeft because "a " ∈ PrefixS("a ") and "a ".trim = "a" ∉ PrefixS("a ".trimLeft)

  
  def ⊑(that: Prefix): Boolean = that match {
    case PrefixS(t) => s.startsWith(t)
    case _ ⇒ false
  }

  def toConstraint(v: String) = StringConstraint(StringVar(v), StringComparator.startsWith, StringConst(s))
}
