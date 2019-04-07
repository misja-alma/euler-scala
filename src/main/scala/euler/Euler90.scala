package euler

object Euler90 extends App {
  // First cube: try all 6 out of 10 permutations. But filter so that for all squares there is at least one nr present.
  // Second cube: start with all the missing nrs. The rest can be random from the remaining nrs not taken by the 2nd cube.
  // Count all distinct arrangements that can make all squares. Note: we might not have to explicitly count the random remainders of the 2nd cube,
  // possibly they don't matter at all so we can just calculate their amount and add them all at once.
  // Note: for counting purposes, 6 is distinct from 9.
  // Question: are the cubes ordered or not, for the purpose of counting distinct arrangements?
  // For convenience sake, let's count all; if they are unordered then the result has to be halved.

  lazy val squares = Set[Seq[Int]](Seq(0, 1), Seq(0, 4), Seq(0, 9), Seq(1, 6), Seq(2, 5), Seq(3, 6), Seq(4, 9), Seq(6, 4), Seq(8, 1))

  // Take 'take' nrs from from 0 .. (total - 1)
  def permutations(total: Int, take: Int): Seq[Seq[Int]] = (0 until total).combinations(take).toSeq

  lazy val takeValid6OutOf10 = permutations(10, 6).filter(allSquaresHit)

  def expand6and9s(ints: Seq[Int]): Seq[Int] = {
    val asSet = ints.toSet
    if (asSet.contains(6)) (asSet + 9).toSeq else if (asSet.contains(9)) (asSet + 6).toSeq else ints
  }                                                                                                                          

  // 6 or 9's are also hit by their upside down partners
  def nrsHit(target: Seq[Int], hitters: Seq[Int]): Int = {
    val expanded = expand6and9s(hitters)
    target.intersect(expanded).size
  }

  def allSquaresHit(numbers: Seq[Int]): Boolean = squares.forall(s => nrsHit(s, numbers) >= 1)

  def allSquaresCovered(cube1: Seq[Int], cube2: Seq[Int]): Boolean = {
    val cube1Set = expand6and9s(cube1).toSet
    val cube2Set = expand6and9s(cube2).toSet
    squares.forall(s => cube1Set(s.head) && cube2Set(s.last) || cube2Set(s.head) && cube1Set(s.last))
  }

  def possibleSecondCubes(firstCube: Seq[Int]): Int = {
    // determine missing nrs first as y. Then there are 10 - y nrs left to take freely. So the result is 10! / y!
    // note that there can be multiple choices for a nr when cube1 covers a square with 2 nrs. So let's just brute force it.
    takeValid6OutOf10
      .count(secondCube => allSquaresCovered(firstCube, secondCube))
  }

  val solution = takeValid6OutOf10.map(possibleSecondCubes).sum

  // Since it's not clear if the cubes are ordered, print the solution for both  (Note: it was the second choice).
  println ("Solution: " + solution + ", or: " + (solution / 2))
}
