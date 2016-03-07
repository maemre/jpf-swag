package edu.ucsb.cs.jpf.swag.helpers
import gov.nasa.jpf.symbc.numeric
import gov.nasa.jpf.symbc.string

import scala.collection.JavaConversions._

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

case class ParseError(message: String) extends Exception(message)

sealed trait ID
case class Name(s: String) extends ID {
  override def toString = s"$s"
}
case class StackID(i: Int) extends ID {
  override def toString = s"stack_$i"
}

object Helpers {
  import edu.ucsb.cs.jpf.swag.constraints._
  import gov.nasa.jpf.symbc.mixednumstrg

  /**
    * Find ID corresponding to the symbol.
    */
  def ι(x: String): ID = if (x.startsWith("stack_")) {
    StackID(x.substring("stack_".length).toInt)
  } else {
    Name(x) // x.substring("sym_".length))
  }

  def ι(i: Int): ID = StackID(i)

  def parsePC(pc: numeric.PathCondition): Constraint = {
    val numericPart = Option(pc.header).map(parseNumeric _).toSet
    val stringPart = Option(pc.spc.header).map(parseString _).toSet

    Conjunction(numericPart ++ stringPart)
  }

  def parseNumeric(c: numeric.Constraint): Constraint = c match {
    case c: numeric.LogicalORLinearIntegerConstraints ⇒ Disjunction(iterableAsScalaIterable(c.getList).map(parseNumeric _).toSet)
    case c: numeric.LinearIntegerConstraint ⇒
      val lhs = parseNumExpr(c.getLeft)
      val rhs = parseNumExpr(c.getRight)
      val op = c.getComparator match {
        case numeric.Comparator.EQ ⇒ NumComparator.≡
        case numeric.Comparator.NE ⇒ NumComparator.≠
        case numeric.Comparator.LT ⇒ NumComparator.<
        case numeric.Comparator.LE ⇒ NumComparator.≤
        case numeric.Comparator.GT ⇒ NumComparator.>
        case numeric.Comparator.GE ⇒ NumComparator.≥
      }
      NumericConstraint(lhs, op, rhs)
    case c: numeric.NonLinearIntegerConstraint ⇒
      val lhs = parseNumExpr(c.getLeft)
      val rhs = parseNumExpr(c.getRight)
      val op = c.getComparator match {
        case numeric.Comparator.EQ ⇒ NumComparator.≡
        case numeric.Comparator.NE ⇒ NumComparator.≠
        case numeric.Comparator.LT ⇒ NumComparator.<
        case numeric.Comparator.LE ⇒ NumComparator.≤
        case numeric.Comparator.GT ⇒ NumComparator.>
        case numeric.Comparator.GE ⇒ NumComparator.≥
      }
      NumericConstraint(lhs, op, rhs)
  }

  def parseString(c: string.StringConstraint): Constraint = {
    import string.StringComparator._

    val lhs = parseStrExpr(c.getLeft)
    val rhs = parseStrExpr(c.getRight)
    c.getComparator match {
      case EQ ⇒
        lhs ≡ rhs // TODO: should we change with referential equality?
      case NE ⇒
        Not(lhs ≡ rhs)
      case EQUALS ⇒
        lhs ≡ rhs
      case EQUALSIGNORECASE ⇒
        lhs equalsIgnoreCase rhs
      case NOTEQUALSIGNORECASE ⇒
        Not(lhs equalsIgnoreCase rhs)
      case STARTSWITH ⇒
        lhs startsWith rhs
      case NOTSTARTSWITH ⇒
        Not(lhs startsWith rhs)
      case ENDSWITH ⇒
        lhs endsWith rhs
      case NOTENDSWITH ⇒
        Not(lhs endsWith rhs)
      case CONTAINS ⇒
        lhs contains rhs
      case NOTCONTAINS ⇒
        Not(lhs contains rhs)
      case MATCHES ⇒
        lhs matches rhs
      case NOMATCHES ⇒
        Not(lhs matches rhs)
      case cmp ⇒ throw new MatchError(s"Conversion of comparator $cmp from SPF is not defined.")
    }
  }

  def parseNumOp(op: numeric.Operator): NumBinop.NumBinop = op match {
    case numeric.Operator.PLUS  ⇒ NumBinop.⌜+⌝
    case numeric.Operator.MINUS ⇒ NumBinop.⌜-⌝
    case numeric.Operator.MUL   ⇒ NumBinop.⌜*⌝
    case numeric.Operator.DIV   ⇒ NumBinop.⌜/⌝
    case numeric.Operator.REM   ⇒ NumBinop.⌜%⌝
  }

  def parseNumExpr(e: numeric.IntegerExpression): NumExpr = e match {
    case e:numeric.BinaryLinearIntegerExpression ⇒
      val lhs = parseNumExpr(e.getLeft)
      val rhs = parseNumExpr(e.getRight)
      val op = parseNumOp(e.getOp)
      NumBinopExpr(lhs, op, rhs)
    case e:numeric.BinaryNonLinearIntegerExpression ⇒
      val lhs = parseNumExpr(e.left)
      val rhs = parseNumExpr(e.right)
      val op = parseNumOp(e.op)
      NumBinopExpr(lhs, op, rhs)
    case e:mixednumstrg.SpecialIntegerExpression ⇒
      val str = parseStrExpr(e.opr)
      e.op match {
        case mixednumstrg.SpecialOperator.VALUEOF ⇒ ??? // value of string is not implemented yet
        case mixednumstrg.SpecialOperator.LENGTH ⇒
          Length(str)
        case mixednumstrg.SpecialOperator.INDEXOF ⇒ ??? // couldn't find 2nd argument of indexof 
      }
    case n:numeric.IntegerConstant ⇒ NumConst(n.value)
    case x:numeric.SymbolicInteger ⇒ NumVar(x.getName)
  }

  def parseStrExpr(e: string.StringExpression): StringExpr = e match {
    case s:string.StringConstant ⇒ StringConst(s.value)
    case x:string.StringSymbolic ⇒ StringVar(x.getName)
    case e:string.DerivedStringExpression ⇒
      val lhs = Option(e.left).map(parseStrExpr)
      val rhs = Option(e.right).map(parseStrExpr)
      e.op match {
        case string.StringOperator.CONCAT ⇒
          Concat(lhs.get, rhs.get)
        case string.StringOperator.TRIM ⇒
          Trim(rhs.get)
        case string.StringOperator.TOLOWERCASE ⇒
          ToLowerCase(rhs.get)
        case string.StringOperator.TOUPPERCASE ⇒
          ToUpperCase(rhs.get)
        case _ ⇒ throw new UnsupportedOperationException(s"Unsupported string operation: ${e.op}")
      }
  }

  def removePrime(name: String) =
    if (name.head == '\'') {
      name.tail
    }
    else {
      name
    }

  def addPrime(name: String) = "'" + name
}
