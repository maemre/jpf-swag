package edu.ucsb.cs.jpf.swag.helpers

import scala.collection.JavaConversions.mapAsJavaMap

object ConstraintSolver {
  import com.microsoft.z3._

  val ctx = new Context(Map("model" → "true"))
  val solver = ctx.mkSolver

  def solveConstraints(constraints: Seq[BoolExpr]): Boolean = {
    solver.reset()
    constraints.foreach {
      solver.add(_)
    }
    solver.check == Status.SATISFIABLE
  }

  def parseSMTLIB2String(formula: String): BoolExpr = {
    ctx.parseSMTLIB2String(formula, null, null, null, null)
  }
}
