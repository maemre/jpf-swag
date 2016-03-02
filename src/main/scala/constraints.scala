package edu.ucsb.cs.jpf.swag.constraints

import gov.nasa.jpf.symbc.numeric

case class ProperConstraint(left: numeric.Expression, comp: numeric.Comparator, right: numeric.Expression) extends numeric.Constraint(left, comp, right) {

  def eqchk[A](l:A, r:A) = if (l == null) r == null else l.equals(r)

  override def equals(o: Any): Boolean = o match {
    case that:numeric.Constraint ⇒
      println(s"------------------ $this == $that")
      eqchk(left, that.getLeft) && eqchk(comp, that.getComparator) && eqchk(right, that.getRight)
    case _ ⇒ false
  }

  override def not = ???
}

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
}
case class Not(c: Constraint) extends Constraint {
  // use law of double negation for simplification
  // I don't know whether this is valid with string constraints
  override def unary_~ = c
}
case class Disjunction(disjuncts: Set[Constraint]) extends Constraint {
  // Specialization to keep formula flatter
  override def |(that:Constraint) = that match {
    case Disjunction(dis) ⇒ Disjunction(disjuncts ++ dis)
    case _ ⇒ Disjunction(disjuncts + that)
  }
}
case class Conjunction(conjuncts: Set[Constraint]) extends Constraint {
  // Specialization to keep formula flatter
  override def &(that:Constraint) = that match {
    case Conjunction(cons) ⇒ Conjunction(conjuncts ++ cons)
    case _ ⇒ Conjunction(conjuncts + that)
  }
}
case object True extends Constraint
case object False extends Constraint
case class BoolVar(x: String) extends Constraint

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

import NumComparator.NumComparator

case class StringConstraint(lhs: StringExpr, op: StringComparator, rhs: StringExpr) extends Constraint {
    override def toString = s"($op $lhs $rhs)"
}
case class NumericConstraint(lhs: NumExpr, op: NumComparator, rhs: NumExpr) extends Constraint {
  override def toString = s"${NumComparator.toString(op)} $lhs $rhs"
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
}

case class StringConst(s: String) extends StringExpr
case class StringVar(name: String) extends StringExpr
case class Concat(lhs: StringExpr, rhs: StringExpr) extends StringExpr
case class Replace(source: StringExpr, matchS: StringExpr, replaceWith: StringExpr) extends StringExpr
case class ReplaceFirst(source: StringExpr, matchS: StringExpr, replaceWith: StringExpr) extends StringExpr
case class ReplaceAll(source: StringExpr, matchS: StringExpr, replaceWith: StringExpr) extends StringExpr
case class Trim(expr: StringExpr) extends StringExpr
case class Substring(str: StringExpr, idx: NumExpr) extends StringExpr
case class ValueOf(n: NumExpr) extends StringExpr
case class ToUpperCase(str: StringExpr) extends StringExpr
case class ToLowerCase(str: StringExpr) extends StringExpr
case object NoStringExpr extends StringExpr

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

import NumBinop.NumBinop

object NumUnop extends Enumeration {
  type NumUnop = Value
  val negate, increment, decrement = Value
}

import NumUnop.NumUnop

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
}
case class Length(expr: StringExpr) extends NumExpr
case class IndexOf(s: StringExpr, c: CharExpr) extends NumExpr
case class LastIndexOf(s: StringExpr, c: CharExpr) extends NumExpr
case class NumVar(name: String) extends NumExpr
case class NumConst(n: BigInt) extends NumExpr
case class NumBinopExpr(lhs: NumExpr, op: NumBinop, rhs: NumExpr) extends NumExpr
case class NumUnopExpr(op: NumUnop, num: NumExpr) extends NumExpr
case object NoNumExpr extends NumExpr
