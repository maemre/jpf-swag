package edu.ucsb.cs.jpf.swag.domains

import edu.ucsb.cs.jpf.swag.constraints._
import edu.ucsb.cs.jpf.swag.helpers._
import scala.annotation.tailrec
import scala.collection.mutable.{Map ⇒ MMap, Set ⇒ MSet, StringBuilder}

import apron._

import Helpers.ι

// problem is getting from our constraints to
// apron and back

class ApronRelationalNumberDomain
()
extends RelationalNumber[ApronRelationalNumberDomain] {
  def ⊔(that: ApronRelationalNumberDomain): ApronRelationalNumberDomain = {
    ???
  }

  override def ∇(that: ApronRelationalNumberDomain) = this ⊔ that

  def ⊑(that: ApronRelationalNumberDomain): Boolean = {
    ???
  }

  def construct(constraints: Conjunction) = {
    this
  }

  def projectTo(id: ID): Option[Constraint]  = {
    ???
  }

  def projectOut(id: ID): ApronRelationalNumberDomain = {
    ???
  }

  // add extra constraint
  def addConstraint(c: Constraint): ApronRelationalNumberDomain = {
    ???
  }

  // Constraint representation of this abstract domain
  def toConstraint: Constraint = ???
}
