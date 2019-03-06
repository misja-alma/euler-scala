package euler

import euler.Utils.PrimeCache

object Euler60 extends App {

  implicit val cache = new PrimeCache[Long](2, 3)

  def isPrimePair(pair: (Long, Long)): Boolean = {
    val (x, y) = pair
    Utils.isPrime(Utils.concat(x.toInt, y.toInt)) && Utils.isPrime(Utils.concat(y.toInt, x.toInt))
  }

  def isValidCombination(p1: PrimeCombi, p2: PrimeCombi): Boolean = {
    p1.elements.forall(p2.isValidNewElement)
  }

  case class PrimeCombi(elements: Set[Long]) {
    override def equals(obj: Any): Boolean = obj.isInstanceOf[PrimeCombi] && obj.asInstanceOf[PrimeCombi].elements == elements

    override def hashCode(): Int = elements.hashCode()

    def isValidNewElement(p: Long): Boolean = !elements.contains(p) && elements.forall(e => isPrimePair((e, p)))

    lazy val sum: Long = elements.sum
  }

  type PairPool = Vector[PrimeCombi]

  implicit val primeCombiOrdering = new Ordering[PrimeCombi] {
    override def compare(x: PrimeCombi, y: PrimeCombi): Int = x.sum compare y.sum
  }

  val finalState = Iterator.iterate(State(Utils.primes, Vector[Long](), Vector[PrimeCombi](), Vector[PrimeCombi](), Vector[PrimeCombi]()))(handleNewPrime)
    .dropWhile(_.p5.isEmpty)
    .next()

  case class State(pStream: Iterator[Long], p1: Vector[Long], p2: PairPool, p4: PairPool, p5: PairPool)

  def handleNewPrime(s: State): State = {
    val nextPrime: Long = s.pStream.next()
    println ("Next Prime: " + nextPrime)

    val newPairs = s.p1
      .filter(p => isPrimePair(nextPrime, p))
      .map(p => PrimeCombi(Set(p, nextPrime)))
    val newP2 = s.p2 ++ newPairs

    val newQuints = s.p4
      .filter(_.isValidNewElement(nextPrime))
      .map(p4 => PrimeCombi(p4.elements + nextPrime))
      .toSet
    val newP5 = s.p5 ++ newQuints

    val newQuads = Utils.cross(newPairs, s.p2)
      .filter { case (p1, p2) => isValidCombination(p1, p2) }
      .map { case (p1, p2) => PrimeCombi(p1.elements ++ p2.elements) }
      .toSet
    val newP4 = s.p4 ++ newQuads

    if (newQuads.nonEmpty) println ("New quads: " + newQuads)

    val newP1 = s.p1 :+ nextPrime

    s.copy(p1 = newP1, p2 = newP2, p4 = newP4, p5 = newP5)
  }

  println ("Solution: " + finalState.p5.head.sum)
}
