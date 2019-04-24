package euler

import scala.io.Source

import Utils._

object Euler98 extends App {
  // max length is 14 chars, max anagram length is 9 chars, 42 word anagram groups and 7118 squares
  val words = Source.fromInputStream(getClass.getResourceAsStream("/p098_words.txt"))
    .getLines()
    .next()
    .split(",")
    .map(_.init.tail)

  val primes = Seq[Long](2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101).toArray

  def primeProduct(s: String): Long = s.map(_ - 'A').map(primes).product

  val anagrams = words
    .groupBy(primeProduct)
    .filterNot(_._2.length == 1)
    .values.toList
    .sortBy(_.head.length)

  val squares = (4L to Math.sqrt(999999999).toLong).map(x => x * x).map(digits)

  def digitPrimeProduct(x: Seq[Int]): Long = x.map(primes).product

  val anagramSquares = squares
    .groupBy(digitPrimeProduct)
    .filterNot(_._2.length == 1)
    .values.toList
    .groupBy(_.head.length)

  def getMatchingSquareAnagrams(an1: String, an2: String): Seq[(Long, Long)] = {
    val candidates = anagramSquares.getOrElse(an1.length, Seq())
    candidates.flatMap(getAllMatches(an1, an2))
  }

  def getAllMatches(an1: String, an2: String)(squareAnagramGroup: Seq[Seq[Int]]): Seq[(Long, Long)] =
    if (squareAnagramGroup.isEmpty) Seq() else {
      val matchesOnHead = for {
        digitMap <- mapDigits(an1, squareAnagramGroup.head)
      } yield squareAnagramGroup.tail.flatMap(getMatchingSecondSquare(digitMap, an2))

      matchesOnHead.toSeq.flatten.map(x => (fromDigits(squareAnagramGroup.head), x)) ++ getAllMatches(an1, an2)(squareAnagramGroup.tail)
    }

  def mapDigits(str: String, digits: Seq[Int]): Option[Map[Char, Int]] = {
    val (error, mappings) = str.zip(digits).foldLeft((false, Map[Char, Int]())) { case ((err, mapp), (c, d)) =>
      if (err) (err, mapp) else {
        if (mapp.contains(c)) {
          (mapp(c) != d, mapp)
        } else {
          if (mapp.values.exists(_ == d)) {
            (true, mapp)
          } else {
            (false, mapp + (c -> d))
          }
        }
      }
    }

    if (error) None else Some(mappings)
  }

  def getMatchingSecondSquare(digitMap: Map[Char, Int], str: String)(digits: Seq[Int]): Option[Long] =
    if (str.zip(digits).forall { case (c, d) => digitMap(c) == d }) Some(fromDigits(digits)) else None

  val allPairs = anagrams.flatMap(anagramGroup =>
    anagramGroup.combinations(2).flatMap(ar => getMatchingSquareAnagrams(ar(0), ar(1)))
  )

  val maxAnagramSquare = allPairs.flatMap { case (x, y) => Seq(x, y) }.max

  println("Solution: " + maxAnagramSquare)

}
