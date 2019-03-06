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
}
