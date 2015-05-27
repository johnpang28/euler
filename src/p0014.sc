import scala.annotation.tailrec

/*
Longest Collatz sequence

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved
yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?
NOTE: Once the chain starts the terms are allowed to go above one million.
 */
object p0014 {

  // Solution is too slow without memoisation, so keep cache of lengths of known Collatz sequences.
  // Keeping just the length is more efficient than keeping the whole sequence.
  // Unfortunately, the code is now imperative rather than functional. :(
  var knownChainLengths: Map[Long, Int] =
    Map(1L -> 1, 2L -> 2, 4L -> 3, 8L -> 4, 16L -> 5, 5L -> 6, 10L -> 7, 20L -> 8, 40L -> 9, 13L -> 10)

  def next(x: Long): Long = if (x % 2 == 0) x/2 else 3*x + 1

  @tailrec
  def collatzSeq(xs: List[Long]): List[Long] =
    if (xs.last == 1) xs
    else {
      val n = next(xs.last)
      knownChainLengths.get(n) match {
        case Some(s) => xs ::: List(n)
        case None => collatzSeq(xs ::: List(n))
      }
    }

  def collatzChainLength(x: Long): Int = {
    val c = collatzSeq(List(x))
    if (c.last == 1) knownChainLengths += (x -> c.length)
    else for (i <- c.indices) knownChainLengths += (c(i) -> (c.length - i + knownChainLengths(c.last) - 1))
    knownChainLengths(x)
  }

  (3 until 1000000).map(x => (x, collatzChainLength(x.toLong))).sortBy(_._2).last // 837799, 525 terms
}