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

sealed trait Constraint
case class Not(c: Constraint) extends Constraint
case class Disjunction(disjuncts: Set[Constraint]) extends Constraint
case class Conjunction(conjuncts: Set[Constraint]) extends Constraint

object Conjunction {
  def apply(conjuncts: Constraint*): Conjunction = Conjunction(conjuncts.toSet)
}

object StringComparator extends Enumeration {
  type StringComparator = Value
  val ⌜==⌝, eq, equalsIgnoreCase, startsWith, endsWith, contains, isInteger, isLong, isDouble, isBoolean, empty, matches, regionMatches = Value
}

import StringComparator.StringComparator

object NumComparator extends Enumeration {
  type NumComparator = Value
  val ≡, ≠, <, ≤, >, ≥ = Value
}

import NumComparator.NumComparator

case class StringConstraint(lhs: StringExpr, op: StringComparator, rhs: StringExpr) extends Constraint
case class NumericConstraint(lhs: NumExpr, op: NumComparator, rhs: NumExpr) extends Constraint

sealed trait StringExpr
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

sealed trait NumExpr
case class Length(expr: StringExpr) extends NumExpr
case class IndexOf(s: StringExpr, c: CharExpr) extends NumExpr
case class LastIndexOf(s: StringExpr, c: CharExpr) extends NumExpr
case class NumVar(name: String) extends NumExpr
case class NumConst(n: BigInt) extends NumExpr
case class NumBinopExpr(lhs: NumExpr, op: NumBinop, rhs: NumExpr) extends NumExpr
case class NumUnopExpr(op: NumUnop, num: NumExpr) extends NumExpr
