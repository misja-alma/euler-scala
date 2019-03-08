package euler

object Euler68 extends App {
  // Make 5 triples with each (min. 13, max. 20) as total
  // concatenate, the result has 16 digits.
  // Way to represent the 5-gon ring:
  // We want to be able to:
  // - enumerate the triplets starting from the outside (clockwise).
  //   This we want both for creating the string representation, and for finding all possible solutions:
  //   for this we start from the outside of the 1st triplet, enumerate, then take the 2nd node, etc, then the outside of the next triplet etc.
  // - for each node, find the triplets it belongs to
  // So we could number the nodes, number the triplets, maybe keep a multimap of triplets per node, and keep a list of nodes per triplet (ordered outside to inside).
  // Then: for each sum (13-20): for each triplet: for each node: try all remaining nrs 1-10 that suit; next until either no nr suits (go back) or all nrs finished: add solution.
  // Order all solutions alphabetically and take the max.
  // Note that the same algo works for 3-gon as well, only the cutting points of the triplets are different.

  val solutionLength = 16
  val maxNr = 10
  val gones = 5

  // Node index within triplets: triplet(i / 3)(i % 3)
  // The actual node values per triplet
  // a value of 0 in the triplets is recognized as an empty value
  implicit val triplets: Array[Array[Int]] = new Array[Array[Int]](gones).map(_ => new Array[Int](3))

  def isFree(index: Int): Boolean = triplets(index / 3)(index % 3) == 0

  // the value has to be set in every crossing triplet!
  def setValue(index: Int, value: Int)(implicit trips: Array[Array[Int]]): Unit = {
    val gns = trips.length
    trips(index / 3)(index % 3) = value
    index % 3 match {
      case 1 =>
        val trip = (index / 3 + (gns - 1)) % gns
        trips(trip)(2) = value
      case 2 =>
        val trip = (index / 3 + 1) % gns
        trips(trip)(1) = value
      case 0 => // nothing
    }
  }

  def hasValidSum(sum: Int)(triplet: Int): Boolean = {
    triplets(triplet).sum == sum || triplets(triplet).contains(0)
  }

  def makeSolution: String = {
    // start at the -numerically- lowest triplet
    val lowest = triplets.indices.minBy(triplets(_)(0))
    (lowest until (lowest + gones)).flatMap { t => triplets(t % gones).map(_.toString)}.mkString
  }

  // For the node index, gives the triplets (indices) that it is in
  def tripletsPerNode(nodeIndex: Int): Seq[Int] = {
    nodeIndex % 3 match {
      case 0 => Seq(nodeIndex / 3)
      case 1 => Seq(nodeIndex / 3, (nodeIndex / 3 + (gones - 1)) % gones)
      case _ => Seq(nodeIndex / 3, (nodeIndex / 3 + 1) % gones)
    }
  }

  val allSolutions = ((3 + maxNr) to (2 * maxNr)) flatMap getSolutionsWithSum

  val solution = allSolutions.filter(_.length == solutionLength).max

  println ("Solution: " + solution)

  def getSolutionsWithSum(sum: Int): Set[String] = {

    def doGetSolutions(nodeIndex: Int, nrsLeft: Set[Int]): Set[String] = {
      if (nrsLeft.isEmpty) Set(makeSolution) else {
        if (isFree(nodeIndex)) {
          val solutions = nrsLeft.flatMap { nr =>
            setValue(nodeIndex, nr)
            if (tripletsPerNode(nodeIndex).forall(hasValidSum(sum))) {
              doGetSolutions(nodeIndex + 1, nrsLeft - nr)
            } else {
              Set[String]()
            }
          }
          setValue(nodeIndex, 0)
          solutions
        } else {
          doGetSolutions(nodeIndex + 1, nrsLeft) 
        }
      }
    }

    doGetSolutions(0, (1 to maxNr).toSet)
  }

}
