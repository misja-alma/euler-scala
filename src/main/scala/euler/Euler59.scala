package euler

import scala.io.Source

object Euler59 extends App {
  // questions: the cipher is 3 lower case characters
  // - does that mean that the text is encoded in blocks of 3 characters at a time?
  // - are spaces encoded as well or are they stripped?
  // - does the text maybe contain punctuation symbols as well?

  // strategies:
  // - do a frequency count and deduce some characters based on the average frequencies of characters in English
  // - or try out all possibilities until the decoded text contains only English words from a dictionary. Or maybe 90% should be words

  val text = Source.fromInputStream(getClass.getResourceAsStream("/p059_cipher.txt")).getLines().next().split(",").map(_.toInt).toList

  val firstChars = text.sliding(3, 3).map(_.head).toSeq
  val secondChars = text.sliding(3, 3).filter(_.length > 1).map(_(1)).toSeq
  val thirdChars = text.sliding(3, 3).filter(_.length > 2).map(_(2)).toSeq
  
  val freqs = firstChars.groupBy(identity).mapValues(_.length)

  println ("Frequencies:")

  val sortedByFreq = freqs.toList.sortBy(_._2).reverse.map { case (k, v) => (k.toInt, v)}

  println(sortedByFreq)

  // println (sortedByFreq.map { case (k, v) => (k ^ key).toChar})

  // algo: assume the above 5 chars are in the text.
  // Maybe assume also that the 3 char cypher is 3 times the same char, because the freqs are every time the same. But we could do without this assumption and take only 1st chars first.
  // Then try out all possible encrypted chars and see if there is a key that comes out that fits to all other chars as well.

  // So for each letter: map to the set of possible cyphers. Then find the key(s) that are present in every set.

  // 1st char: cypher is 94
  // 2nd and 3rd char: also 94 ?!
  val chars = Seq('e', 'r', 'o', 'i', 'a', 's', 't', 'n')
  val encodedCharSet = thirdChars.toSet

  val punctuations = Set(' ', ',', '.', ':', ';', '!', '?', ''', '"', '-', '(', ')').map(_.toInt)

  def isValidChar(i: Int): Boolean = i.toChar.isLetterOrDigit || punctuations.contains(i)

  def possibleCyphers(shouldBePresent: Char, encoded: Set[Int]): Set[Int] = {
    // filter encoded:
    // for each char, xor with sbp; test if the result is a valid cypher by xor'ing it will all chars in encoded and verifying that the result is in a..z
    encoded.flatMap { c =>
      val keyCandidate: Int = c ^ shouldBePresent
      if (keyCandidate.toChar.isValidChar && keyCandidate.toChar.isLower && encoded.forall(ec => isValidChar(ec ^ keyCandidate))) Some(keyCandidate) else None
    }
  }

  val setsOfPossibleCyphers: Seq[Set[Int]] = chars.map { c => possibleCyphers(c, encodedCharSet) }

  val possibleForAll = setsOfPossibleCyphers.head.filter { c =>
    setsOfPossibleCyphers.forall(_.contains(c))
  }

  println ("Possible cyphers: " + possibleForAll)

  // key 1st char: 103
  // key 2nd char: 111
  // key 3rd char: 100
  val key = 103

  val valid = firstChars.forall{ c => isValidChar(c ^ key) }
  println ("Valid: " + valid)
  println (firstChars.map(_ ^ key).map(_.toChar).mkString)

  val keys = Seq(103, 111, 100)
  val result = text.sliding(3, 3).flatMap(decode).mkString

  def decode(ints: Seq[Int]): String = ints.zip(keys).map { case (c, k) => (c ^ k).toChar }.mkString

  println (result)

  println ("Solution: " + result.map(_.toInt).sum)
}
