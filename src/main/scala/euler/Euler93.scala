package euler

import scala.annotation.tailrec

object Euler93 extends App {
  // Just enumerate for each 4 out of 10 all possible expressions and keep a set of their outcomes.
  // Note that the enumeration of the expressions is every time the same, only the digits change. So we could be a bit clever with caching or so.
  // First group into squares; take all possible subsets so 2^4 * x possibilities. This x is because each subset should also be grouped again.
  // Then combine all sets using each possible operator. Note that there are 3 combinations and 4 possible operators. So total combi's is 16 * 3 * 4 * x = 192x, say 400 or so.
  // Maybe all these combinations could be kept as functions with 4 parameters because we need them 10 over 6 times 4!.  (5040 times)
  // Then iterate over all permutations of the 4 taken numbers:
  // calculate each result as a fraction, and use only whole results.
  // Note we probably don't want to use zeroes to avoid div by 0 etc.

  lazy val allOperators = Seq(Plus, Minus, Mul, Div)

  def createAllExpressions(nrPlaceHolders: Int): Seq[Expression] = {
    // make for each operator a compound expression of 2 placeholders.
    // Then make compound expressions by combining with all operators both from left and right and also with or without parentheses when applicable.
    // Do this once more and we have the result.

    def combineLeft(value: PlaceHolderValue, operator: Operator, expressions: Seq[CompoundExpression]): Seq[CompoundExpression] = {
      expressions.flatMap { exp =>
        val simpleCombination = CompoundExpression(value +: exp.variables, operator +: exp.operators)
        val leftWithoutBrackets = if (operator.hasPrecedence || !operator.isCommutative) {
          // a+(b+c)
          Seq(CompoundExpression(Seq(value, exp), Seq(operator)), simpleCombination)
        } else {
          // a+b+c
          Seq(simpleCombination)
        }

        // (a+b)+c
        val leftWithBrackets = if (exp.operators.isEmpty) Seq() else {
          val rightExp = CompoundExpression(exp.variables.tail, exp.operators.tail)
          val leftExp = CompoundExpression(Seq(value, exp.variables.head), Seq(operator))
          Seq(CompoundExpression(Seq(leftExp, rightExp), Seq(exp.operators.head)))
        }

        leftWithoutBrackets ++ leftWithBrackets
      }
    }

    def combineRight(value: PlaceHolderValue, operator: Operator, expressions: Seq[CompoundExpression]): Seq[CompoundExpression] = {
      expressions.flatMap { exp =>
        val simpleCombination = CompoundExpression(value +: exp.variables, operator +: exp.operators)
        val rightWithoutBrackets = if (operator.hasPrecedence) {
          // (a+b)+c
          Seq(CompoundExpression(Seq(value, exp), Seq(operator)), simpleCombination)
        } else {
          // a+b+c
          Seq(simpleCombination)
        }

        // a+(b+c)
        val righttWithBrackets = if (exp.operators.isEmpty) Seq() else {
          val leftExp = CompoundExpression(exp.variables.init, exp.operators.init)
          val rightExp = CompoundExpression(Seq(exp.variables.last, value), Seq(operator))
          Seq(CompoundExpression(Seq(leftExp, rightExp), Seq(exp.operators.head)))
        }

        rightWithoutBrackets ++ righttWithBrackets
      }
    }

    if (nrPlaceHolders == 1) Seq(PlaceHolderValue(0)) else {
      @tailrec
      def addCombinationsForPlaceHolders(exprSoFar: Seq[CompoundExpression], currentPlaceHolder: Int): Seq[Expression] = {
        if (currentPlaceHolder >= nrPlaceHolders) exprSoFar else {
          val p = PlaceHolderValue(currentPlaceHolder)
          val newExpr = allOperators.flatMap(op => combineLeft(p, op, exprSoFar)) ++ allOperators.flatMap(op => combineRight(p, op, exprSoFar))
          addCombinationsForPlaceHolders(newExpr, currentPlaceHolder + 1)
        }
      }

      val p0 = PlaceHolderValue(0)
      val p1 = PlaceHolderValue(1)
      val expr2 = allOperators.map(op => CompoundExpression(Seq(p0, p1), Seq(op)))

      addCombinationsForPlaceHolders(expr2, 2)
    }
  }

  // Take a shortcut here, assume that no longest sequence starts above 100. And of course it should start above 0.
  def longestSequence(nrs: Set[Int]): (Int, Int) = {
    var start = 1
    var longest = 0
    var bestStart = 0

    while (start < 100) {
      var localLongest = 0
      var i = start
      while (nrs(i)) {
        localLongest += 1
        i += 1
      }

      if (localLongest > longest) {
        longest = localLongest
        bestStart = start
      }
      start = i + 1
    }

    (bestStart, longest)
  }

  val valueArray = new Array[Int](4)
  val allExpressions = createAllExpressions(4)

  val groupsWithLongestSequence = (1 to 9).combinations(4).map {nrs =>
    val allResults = nrs.indices.permutations.flatMap { useIndices =>
      useIndices.zip(nrs).foreach { case (i, number) =>
        valueArray(i) = number
      }
      allExpressions.map(_.calculate(valueArray: _*))
        .filterNot(_.denominator == 0)
        .filter(_.isWholeNumber)
        .map(_.toIntegral)
    }.toSet

    (nrs, longestSequence(allResults))
  }.toList

  val (group, (start, length)) = groupsWithLongestSequence.maxBy { case (_, (_, len)) => len}

  println (s"Solution: ${group.sorted.mkString} with length $length starting at $start")
}
