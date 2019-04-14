package euler

import fastparse._, NoWhitespace._

object Expression {

  /**
    * Syntax: E.g: $1 * ($2 + $3)
    */
  def parseExpression(s: String): Expression = {
    def placeHolder[_: P]: P[PlaceHolderValue] = P("$" ~ CharIn("0-9").rep(1).!).map(nr => PlaceHolderValue(nr.toInt))
    def plus[_: P]: P[Operator] = P("+").map(_ => Plus)
    def minus[_: P]: P[Operator] = P("-").map(_ => Minus)
    def mul[_: P]: P[Operator] = P("*").map(_ => Mul)
    def div[_: P]: P[Operator] = P("/").map(_ => Div)
    def operator[_: P]: P[Operator] = plus | minus | mul | div
    def bareExpression[_: P]: P[Expression] = parenthesised | placeHolder
    def parenthesised[_: P]: P[Expression] = P("(" ~/ expression ~ ")")
    def expression[_: P]: P[Expression] = (bareExpression  ~ (operator ~ bareExpression).rep).map { case (exp, opExpSeq) =>
      if (opExpSeq.isEmpty) exp else CompoundExpression(exp +: opExpSeq.map(_._2), opExpSeq.map(_._1))}

    val Parsed.Success(result, _) = parse(s, expression(_))

    result
  }
}

object Plus extends Operator {
  override def hasPrecedence: Boolean = false
  override def isCommutative: Boolean = true

  override def apply(lhs: Fraction[Int], rhs: Fraction[Int]): Fraction[Int] = lhs + rhs

  override def toString: String = "+"
}

object Minus extends Operator {
  override def hasPrecedence: Boolean = false
  override def isCommutative: Boolean = false

  override def apply(lhs: Fraction[Int], rhs: Fraction[Int]): Fraction[Int] = lhs - rhs

  override def toString: String = "-"
}

object Mul extends Operator {
  override def hasPrecedence: Boolean = true
  override def isCommutative: Boolean = false

  override def apply(lhs: Fraction[Int], rhs: Fraction[Int]): Fraction[Int] = lhs * rhs

  override def toString: String = "*"
}

object Div extends Operator {
  override def hasPrecedence: Boolean = true
  override def isCommutative: Boolean = false

  override def apply(lhs: Fraction[Int], rhs: Fraction[Int]): Fraction[Int] = lhs / rhs

  override def toString: String = "/"
}

trait Expression {
  def calculate(placeHolderValues: Int*): Fraction[Int]
}

trait Operator {
  def hasPrecedence: Boolean
  def isCommutative: Boolean

  def apply(lhs: Fraction[Int], rhs: Fraction[Int]): Fraction[Int]
}

case class CompoundExpression(variables: Seq[Expression], operators: Seq[Operator]) extends Expression {

  override def calculate(placeHolderValues: Int*): Fraction[Int] = {
    // +, - delay the calculation until right hand is resolved, * and / immediately combine with the argument on the right
    def doCalculate(lhs: Fraction[Int], ops: Seq[Operator], exps: Seq[Expression]): Fraction[Int] = {
      if (ops.isEmpty) lhs else {
        val operator +: remainingOps = ops
        val exp +: remainingExps = exps

        if (operator.hasPrecedence) doCalculate(operator(lhs, exp.calculate(placeHolderValues: _*)), remainingOps, remainingExps)
        else operator(lhs, doCalculate(exp.calculate(placeHolderValues: _*), remainingOps, remainingExps))
      }
    }

    doCalculate(variables.head.calculate(placeHolderValues: _*), operators, variables.tail)
  }

  override def toString: String = "(" + variables.head + variables.tail.zip(operators).map{ case (v, op) => op.toString + v.toString}.mkString + ")"
}

case class PlaceHolderValue(placeHolder: Int) extends Expression {
  override def calculate(placeHolderValues: Int*): Fraction[Int] = Fraction(placeHolderValues(placeHolder))

  override def toString: String = s"$$$placeHolder"
}