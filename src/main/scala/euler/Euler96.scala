package euler

import scala.io.Source

object Euler96 extends App {
  val lines = Source.fromInputStream(getClass.getResourceAsStream("/p096_sudoku.txt")).getLines().toList

  val sudokus = lines.sliding(10, 10).map(block => parseSudoku(block.tail))

  val solutions = sudokus.map(solveSudoku).map(_.head)

  val solution = solutions.map(s => s(0)(0) * 100 + s(0)(1) * 10 + s(0)(2)).sum

  println("Solution: " + solution)


  type Sudoku = Array[Array[Int]]

  def parseSudoku(rows: List[String]): Sudoku = rows.map(_.map(_.asDigit).toArray).toArray

  def solveSudoku(inputs: Sudoku): Seq[Sudoku] = {

    def findSolutions(guesses: Sudoku, currentField: Int): Seq[Sudoku] = {
      val (row, col) = (currentField / 9, currentField % 9)
      if (row >= 9) Seq(clone(guesses)) else {
        if (inputs(row)(col) == 0) {
          val solutions = (1 to 9)
            .filterNot(guessGivesConflict(guesses, row, col))
            .flatMap { guess =>
              guesses(row)(col) = guess
              findSolutions(guesses, currentField + 1)
            }
          guesses(row)(col) = 0
          solutions
        } else {
          findSolutions(guesses, currentField + 1)
        }
      }
    }

    findSolutions(clone(inputs), 0)
  }

  def guessGivesConflict(given: Sudoku, row: Int, col: Int)(guess: Int): Boolean = {
    given(row).contains(guess) || given.exists(_ (col) == guess) || {
      val blockStart = row - row % 3
      given.slice(blockStart, blockStart + 3).exists { row =>
        val colStart = col - col % 3
        row.slice(colStart, colStart + 3).contains(guess)
      }
    }
  }

  def clone(sudoku: Sudoku): Sudoku = sudoku.map(_.clone)

}
