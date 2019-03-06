package euler

object Euler61 extends App {
  // Create a graph of all 4 digit nrs and their connections (tail to head, one direction)
  // then find a cycle of length 6 that visits all different series     (hamilton cycle?)
  // One way to do it: try every node as starting node, then just do dfs
  // When one -starting- node doesn't work, it cannot be included in next searches anymore
  // Note that we have to only find the first success.
  // To speed up the search, there could be a hashmap with key the 2 starting digits of all nodes
  // each node needs to keep its 2 digit suffix, as well as a key representing its series, and of course the nr itself.
  // So, keep a set of nodes that have been tried as starting node;
  // Keep an iterator that tries every node as a starting node and does a dfs;
  // during each dfs, keep track of the nodes already tried, as well as the nodes in the path so far of course.

  val triangle = (1 until 10000).toStream.map{ x => x * (x + 1) / 2 }
  val square = (1 until 10000).toStream.map{ x => x * x }
  val pentagonal = (1 until 10000).toStream.map{ x => x * (3 * x - 1) / 2 }
  val hexagonal = (1 until 10000).toStream.map{ x => x * (2 * x - 1) }
  val heptagonal = (1 until 10000).toStream.map{ x => x * (5 * x - 3) / 2 }
  val octagonal = (1 until 10000).toStream.map{ x => x * (3 * x - 2) }

  val figurateSeries = List(triangle, square, pentagonal, hexagonal, heptagonal, octagonal)
  
  case class FigurateNumber(figureIndex: Int, number: Int)

  triangle.takeWhile(_ < 10000).toSeq.foreach(println)

  val allNumbers: Seq[FigurateNumber] = figurateSeries.zipWithIndex.flatMap(all4DigitNumbers)

  def all4DigitNumbers(seriesWithIndex: (Stream[Int], Int)): Seq[FigurateNumber] = {
    val (series, index) = seriesWithIndex
    println ("Constructing series " + (index + 3))
    series.dropWhile(_ < 1000).takeWhile(_ <= 9999).map(n => FigurateNumber(index + 3, n))
  }

  println ("Constructing adjacency list ..")

  val adjList: Map[FigurateNumber, Seq[FigurateNumber]] = constructAdjacencyList(allNumbers)

  def isConnected(n1: Int, n2: Int): Boolean =  n1 % 100 == n2 / 100

  def constructAdjacencyList(numbers: Seq[FigurateNumber]): Map[FigurateNumber, Seq[FigurateNumber]] =
    numbers.map{ n => (n, numbers.filter{ n2 => isConnected(n.number, n2.number) }) }.toMap

  val triedStarts = collection.mutable.Set[FigurateNumber]()

  println ("Starting search ..")

  val solution = allNumbers.map{ n => {
    println ("Trying starting nr: " + n)
    triedStarts += n
    get6Cycle(List(n), Set(n))
  } }.find(_.nonEmpty).get.get

  println ("Found cycle: " + solution.map(_.number).reverse)
  println ("Solution: " + solution.map(_.number).sum)

  def sameIndexInList(numbers: List[FigurateNumber], number: FigurateNumber): Boolean = numbers.exists(_.figureIndex == number.figureIndex)

  // Note that tried contains the same els. as soFar..

  def get6Cycle(soFar: List[FigurateNumber], tried: Set[FigurateNumber]): Option[List[FigurateNumber]] = if (soFar.size == 6) Some(soFar) else {
    // find el that connects to head, that is not in tried, not in the triedStarts either, and whose index is not in the soFar. Note: if el is 6th element, it also needs to connect to the last element.
    val connectTo = soFar.head
    val candidates = adjList(connectTo)
      .filterNot(triedStarts)
      .filterNot(tried)
      .filterNot(n => sameIndexInList(soFar, n))

    val finalCandidates = if (soFar.size == 5) {
      candidates.filter{ n => isConnected(n.number, soFar.last.number) }
    } else candidates

    val result = finalCandidates.map { n => get6Cycle( n +: soFar, tried + n) }.filter(_.nonEmpty)

    if (result.isEmpty) None else result.head
  }
}
