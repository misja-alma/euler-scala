package euler

import org.scalatest.{FlatSpec, Matchers}

class Euler96Test extends FlatSpec with Matchers {
  val sudoku1 =
    """003020600
      |900305001
      |001806400
      |008102900
      |700000008
      |006708200
      |002609500
      |800203009
      |005010300""".stripMargin

  import Euler96._

  "solveSudoku" should "return all solutions" in {
    val inputs = parseSudoku(sudoku1.lines.toList)

    solveSudoku(inputs).isEmpty should be (false)
  }

}
