package edu.ucsb.cs.jpf.swag.constraints

import gov.nasa.jpf.symbc.numeric
import gov.nasa.jpf.symbc.string
import gov.nasa.jpf.symbc.mixednumstrg
import edu.ucsb.cs.jpf.swag.helpers.Helpers

sealed trait Constraint {
  /** Negation operator.
   */
  def unary_~ :Constraint = Not(this)

  /** Conjunction operator.
    */
  def &(that: Constraint) = that match {
    case Conjunction(conjuncts) ⇒ Conjunction(conjuncts + this)
    case _ ⇒ Conjunction(this, that)
  }

  /** Disjunction operator.
    */
  def |(that: Constraint) = that match {
    case Disjunction(disjuncts) ⇒ Disjunction(disjuncts + this)
    case _ ⇒ Disjunction(this, that)
  }

  def toSPFConstraint: (Option[numeric.Constraint], Option[string.StringConstraint])

  def addPrime: Constraint
  def removePrime: Constraint
}
case class Not(c: Constraint) extends Constraint {
  // use law of double negation for simplification
  // I don't know whether this is valid with string constraints
  override def unary_~ = c

  def toSPFConstraint = {
    // TODO: specialize for disjunctions to increase precision
    c.toSPFConstraint match {
      case (Some(n), Some(s)) ⇒
        (None, None) // we need to overapproximate ~(s & n), for now we return ⊤
      case (Some(n), None) if n.and != null ⇒
        // we overapproximate ~(p & q) as top again
        (None, None)
      case (Some(n), None) ⇒
        (Some(n.not), None)
      case (None, Some(s)) if s.and != null ⇒
        // we overapproximate ~(p & q) as top again
        (None, None)
      case (None, Some(s)) ⇒
        val s1 = new string.StringConstraint(s.getLeft, s.getComparator.not, s.getRight)
        (None, Some(s1))
      case (None, None) ⇒ ??? // TODO: we need to return bottom in this case
    }
  }

  def addPrime = Not(c.addPrime)
  def removePrime = Not(c.removePrime)
}

case class Disjunction(disjuncts: Set[Constraint]) extends Constraint {
  // Specialization to keep formula flatter
  override def |(that:Constraint) = that match {
    case Disjunction(dis) ⇒ Disjunction(disjuncts ++ dis)
    case _ ⇒ Disjunction(disjuncts + that)
  }

  def toSPFConstraint =
    if (disjuncts.size == 1) {
      disjuncts.head.toSPFConstraint
    } else {
      (None, None)
    }

  def addPrime = Disjunction(disjuncts.map(_.addPrime))
  def removePrime = Disjunction(disjuncts.map(_.removePrime))
}
case class Conjunction(conjuncts: Set[Constraint]) extends Constraint {
  // Specialization to keep formula flatter
  override def &(that:Constraint) = that match {
    case Conjunction(cons) ⇒ Conjunction(conjuncts ++ cons)
    case _ ⇒ Conjunction(conjuncts + that)
  }

  def toSPFConstraint = conjuncts.map(_.toSPFConstraint) reduce { (l, r) ⇒
    val n = (l._1, r._1) match {
      case (Some(l), Some(r)) ⇒
        l.last.and = r
        Some(l)
      case (Some(l), None) ⇒
        Some(l)
      case (None, Some(r)) ⇒
        Some(r)
      case _ ⇒ None
    }

    val s = (l._2, r._2) match {
      case (Some(l), Some(r)) ⇒
        // compute last
        var last = l
        while (last.and != null) {
          last = last.and
        }
        last.and = r
        Some(l)
      case (Some(l), None) ⇒
        Some(l)
      case (None, Some(r)) ⇒
        Some(r)
      case _ ⇒ None
    }

    (n, s)
  }

  def addPrime = Conjunction(conjuncts.map(_.addPrime))
  def removePrime = Conjunction(conjuncts.map(_.removePrime))
}
case object True extends Constraint {
  def toSPFConstraint = (None, None)

  val addPrime = True
  val removePrime = True
}
case object False extends Constraint {
  def toSPFConstraint = ??? // TODO: should be bottom

  val addPrime = False
  val removePrime = False
}

object Conjunction {
  def apply(conjuncts: Constraint*): Conjunction = Conjunction(conjuncts.toSet)
}

object Disjunction {
  def apply(disjuncts: Constraint*): Disjunction = Disjunction(disjuncts.toSet)
}

object StringComparator extends Enumeration {
  type StringComparator = Value
  val ⌜==⌝, eq, equalsIgnoreCase, startsWith, endsWith, contains, isInteger, isLong, isDouble, isBoolean, empty, matches, regionMatches = Value
}

import StringComparator.StringComparator

object NumComparator extends Enumeration {
  type NumComparator = Value
  val ≡, ≠, <, ≤, >, ≥ = Value

  def toString(n: NumComparator): String = n match {
    case ≡ ⇒ "≡"
    case ≠ ⇒ "≠"
    case < ⇒ "<"
    case ≤ ⇒ "≤"
    case > ⇒ ">"
    case ≥ ⇒ "≥"
  }
}

import NumComparator._

case class StringConstraint(lhs: StringExpr, op: StringComparator, rhs: StringExpr) extends Constraint {
  override def toString = s"($op $lhs $rhs)"

  def toSPFConstraint = ???

  def addPrime = copy(lhs.addPrime, op, rhs.addPrime)
  def removePrime = copy(lhs.removePrime, op, rhs.removePrime)
}
case class NumericConstraint(lhs: NumExpr, op: NumComparator, rhs: NumExpr) extends Constraint {
  override def toString = s"${NumComparator.toString(op)} $lhs $rhs"

  def toSPFConstraint = {
    val left = lhs.toSPFExpr
    val right = rhs.toSPFExpr
    val cmp = op match {
      case ≡ ⇒ numeric.Comparator.EQ
      case < ⇒ numeric.Comparator.LT
      case > ⇒ numeric.Comparator.GT
      case ≠ ⇒ numeric.Comparator.NE
      case ≤ ⇒ numeric.Comparator.LE
      case ≥ ⇒ numeric.Comparator.GE
    }

    val constraint = (left, right) match {
      case (_:numeric.LinearIntegerExpression, _:numeric.LinearIntegerExpression) ⇒
        new numeric.LinearIntegerConstraint(left, cmp, right)
      case _ ⇒
        new numeric.NonLinearIntegerConstraint(left, cmp, right)
    }

    (Some(constraint), None)
  }

  def addPrime = copy(lhs.addPrime, op, rhs.addPrime)
  def removePrime = copy(lhs.removePrime, op, rhs.removePrime)
}

sealed trait StringExpr {
  def ⌜==⌝(that: StringExpr) = StringConstraint(this, StringComparator.⌜==⌝, that)
  def ≡(that: StringExpr) = this ⌜==⌝ that
  def eq(that: StringExpr) = StringConstraint(this, StringComparator.eq, that)
  def equalsIgnoreCase(that: StringExpr) = StringConstraint(this, StringComparator.equalsIgnoreCase, that)
  def startsWith(that: StringExpr) = StringConstraint(this, StringComparator.startsWith, that)
  def endsWith(that: StringExpr) = StringConstraint(this, StringComparator.endsWith, that)
  def contains(that: StringExpr) = StringConstraint(this, StringComparator.contains, that)
  def matches(that: StringExpr) = StringConstraint(this, StringComparator.matches, that)

  def concat(that: StringExpr) = Concat(this, that)
  def replace(m: StringExpr, r: StringExpr) = Replace(this, m, r)
  def trim = Trim(this)
  def toUpperCase = ToUpperCase(this)
  def toLowerCase = ToLowerCase(this)
  def charAt(i: NumExpr) = CharAt(this, i)
  def length = Length(this)
  def indexOf(c: CharExpr) = IndexOf(this, c)
  def lastIndexOf(c: CharExpr) = LastIndexOf(this, c)

  def toSPFExpr: string.StringExpression = ???

  def addPrime: StringExpr
  def removePrime: StringExpr
}

case class StringConst(s: String) extends StringExpr {
  def addPrime = this
  def removePrime = this
}
case class StringVar(name: String) extends StringExpr{
  def addPrime = StringVar(Helpers.addPrime(name))
  def removePrime = StringVar(Helpers.removePrime(name))
}
case class Concat(lhs: StringExpr, rhs: StringExpr) extends StringExpr {
  def addPrime = copy(lhs.addPrime, rhs.addPrime)
  def removePrime = copy(lhs.addPrime, rhs.addPrime)
}
case class Replace(source: StringExpr, matchS: StringExpr, replaceWith: StringExpr) extends StringExpr {
  def addPrime = copy(source.addPrime, matchS.addPrime, replaceWith.addPrime)
  def removePrime = copy(source.removePrime, matchS.removePrime, replaceWith.removePrime)
}
case class ReplaceFirst(source: StringExpr, matchS: StringExpr, replaceWith: StringExpr) extends StringExpr {
  def addPrime = copy(source.addPrime, matchS.addPrime, replaceWith.addPrime)
  def removePrime = copy(source.removePrime, matchS.removePrime, replaceWith.removePrime)
}
case class ReplaceAll(source: StringExpr, matchS: StringExpr, replaceWith: StringExpr) extends StringExpr {
  def addPrime = copy(source.addPrime, matchS.addPrime, replaceWith.addPrime)
  def removePrime = copy(source.removePrime, matchS.removePrime, replaceWith.removePrime)
}
case class Trim(str: StringExpr) extends StringExpr {
  def addPrime = copy(str.addPrime)
  def removePrime = copy(str.removePrime)
}
case class Substring(str: StringExpr, idx: NumExpr) extends StringExpr {
  def addPrime = copy(str.addPrime, idx.addPrime)
  def removePrime = copy(str.removePrime, idx.removePrime)
}
case class ValueOf(n: NumExpr) extends StringExpr {
  def addPrime = copy(n.addPrime)
  def removePrime = copy(n.removePrime)
}
case class ToUpperCase(str: StringExpr) extends StringExpr {
  def addPrime = copy(str.addPrime)
  def removePrime = copy(str.removePrime)
}
case class ToLowerCase(str: StringExpr) extends StringExpr {
  def addPrime = copy(str.addPrime)
  def removePrime = copy(str.removePrime)
}
case object NoStringExpr extends StringExpr {
  def addPrime = this
  def removePrime = this
}

sealed trait CharExpr
case class CharConst(c: Char) extends CharExpr
case class CharAt(expr: StringExpr, idx: NumExpr) extends CharExpr

object NumBinop extends Enumeration {
  type NumBinop = Value
  val ⌜+⌝, ⌜-⌝, ⌜*⌝, ⌜/⌝, ⌜%⌝ = Value

  override def toString = this match {
    case ⌜+⌝ ⇒ "+"
    case ⌜-⌝ ⇒ "-"
    case ⌜*⌝ ⇒ "*"
    case ⌜/⌝ ⇒ "/"
    case ⌜%⌝ ⇒ "%"
    case NumBinop ⇒ "NumBinop"
  }
}

import NumBinop._

object NumUnop extends Enumeration {
  type NumUnop = Value
  val negate, increment, decrement = Value
}

import NumUnop._

sealed trait NumExpr {
  def +(that: NumExpr) = NumBinopExpr(this, NumBinop.⌜+⌝, that)
  def -(that: NumExpr) = NumBinopExpr(this, NumBinop.⌜-⌝, that)
  def *(that: NumExpr) = NumBinopExpr(this, NumBinop.⌜*⌝, that)
  def /(that: NumExpr) = NumBinopExpr(this, NumBinop.⌜/⌝, that)
  def %(that: NumExpr) = NumBinopExpr(this, NumBinop.⌜%⌝, that)

  def unary_- = NumUnopExpr(NumUnop.negate, this)
  def ++ = NumUnopExpr(NumUnop.increment, this)
  def -- = NumUnopExpr(NumUnop.decrement, this)

  def <(that:NumExpr) = NumericConstraint(this, NumComparator.<, that)
  def ≤(that:NumExpr) = NumericConstraint(this, NumComparator.≤, that)
  def >(that:NumExpr) = NumericConstraint(this, NumComparator.>, that)
  def ≥(that:NumExpr) = NumericConstraint(this, NumComparator.≥, that)
  def ≡(that:NumExpr) = NumericConstraint(this, NumComparator.≡, that)

  def toSPFExpr: numeric.IntegerExpression

  def addPrime: NumExpr
  def removePrime: NumExpr
}
case class Length(expr: StringExpr) extends NumExpr {
  def toSPFExpr = new mixednumstrg.SpecialIntegerExpression(expr.toSPFExpr, mixednumstrg.SpecialOperator.LENGTH)
  def addPrime = copy(expr.addPrime)
  def removePrime = copy(expr.removePrime)
}
case class IndexOf(s: StringExpr, c: CharExpr) extends NumExpr {
  def toSPFExpr = ???
  def addPrime = copy(s.addPrime)
  def removePrime = copy(s.removePrime)
}
case class LastIndexOf(s: StringExpr, c: CharExpr) extends NumExpr {
  def toSPFExpr = ???
  def addPrime = copy(s.addPrime)
  def removePrime = copy(s.removePrime)
}
case class NumVar(name: String) extends NumExpr {
  def toSPFExpr = new numeric.SymbolicInteger(name)

  def addPrime = NumVar(Helpers.addPrime(name))
  def removePrime = NumVar(Helpers.removePrime(name))
}
case class NumConst(n: Long) extends NumExpr {
  def toSPFExpr = new numeric.IntegerConstant(n)
  def addPrime = this
  def removePrime = this
}
case class NumBinopExpr(lhs: NumExpr, op: NumBinop, rhs: NumExpr) extends NumExpr {
  def toSPFExpr = {
    val left = lhs.toSPFExpr
    val right = rhs.toSPFExpr

    op match {
    case ⌜+⌝ ⇒ left._plus(right)
    case ⌜-⌝ ⇒ left._minus(right)
    case ⌜*⌝ ⇒ left._mul(right)
    case ⌜/⌝ ⇒ left._div(right)
    case ⌜%⌝ ⇒ left._rem(right)
    }
  }

  def addPrime = copy(lhs.addPrime, op, rhs.addPrime)
  def removePrime = copy(lhs.removePrime, op, rhs.removePrime)
}
case class NumUnopExpr(op: NumUnop, num: NumExpr) extends NumExpr {
  def toSPFExpr = op match {
    case NumUnop.negate ⇒ num.toSPFExpr._neg
    case NumUnop.increment ⇒ num.toSPFExpr._plus(1)
    case NumUnop.decrement ⇒ num.toSPFExpr._plus(-1)
  }

  def addPrime = copy(op, num.addPrime)
  def removePrime = copy(op, num.removePrime)
}
case object NoNumExpr extends NumExpr {
  def toSPFExpr = ???

  val addPrime = this
  val removePrime = this
}
