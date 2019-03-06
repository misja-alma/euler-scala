package euler

object Euler62 extends App {
  /*
    * Stream cubes; sort each one's digits and keep them in a map where the value is all the original numbers. Keep streaming until one is added to the map and it is the 5th value.
    * Return the smallest value. Note: this is not the guaranteed solution; it might be that a 6 permut. is found before the smallest 5 permut.
    * TODO what to do with leading zero's? For now assume those are not in the solution ..
    */

  def cube(x: Long): Long = x * x * x

  val cubesPerDigits = collection.mutable.Map[Seq[Int], Seq[Long]]()

  def correctionForLeadingZeroes(ints: Seq[Int]): Int = {
    if (ints.head == 0) {
      val withoutZeroes = ints.dropWhile(_ == 0)
      cubesPerDigits.getOrElse(withoutZeroes, Seq()).size
    } else 0
  }

  def is5Permut(cube: Long): Boolean = {
    val sortedDigits = Utils.digits(cube).sorted
    val existing = cubesPerDigits.getOrElse(sortedDigits, Seq())
    cubesPerDigits.put(sortedDigits, cube +: existing)
    val nrPermuts = 1 + existing.size // + correctionForLeadingZeroes(sortedDigits)
    nrPermuts == 5
  }

  val result = (1L until 100000).toStream.map(cube).find(is5Permut).get

  val allPermuts = cubesPerDigits(Utils.digits(result).sorted)

  println("Solution: " + allPermuts.min)
}
