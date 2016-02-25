package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag.helpers._
import scala.annotation.tailrec
import scala.collection.mutable.{Map ⇒ MMap, Set ⇒ MSet, StringBuilder}

import apron._

import Helpers.ι

// problem is getting from our constraints to
// apron and back

class RelationalNumberDomain
(man: Manager)
extends RelationalNumber[RelationalNumberDomain] {
  def ⊔(that: RelationalNumberDomain): RelationalNumberDomain = {
    ???
  }

  override def ∇(that: RelationalNumberDomain) = this ⊔ that

  def ⊑(that: RelationalNumberDomain): Boolean = {
    ???
  }

  def construct(constraints: Conjunction) = {
    this
  }

  def projectTo(id: ID): Option[Constraint]  = {
    ???
  }

  def projectOut(id: ID): RelationalNumberDomain = {
    ???
  }

  // add extra constraint
  def addConstraint(c: Constraint): RelationalNumberDomain = {
    ???
  }

  // Constraint representation of this abstract domain
  def toConstraint: Constraint = ???

  def toConstraint(v: String): Constraint = ???
}
