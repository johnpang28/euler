import scala.math._

/*

Non-abundant sums

A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28
is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called
abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written
as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers
greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be
reduced any further by analysis even though it is known that the greatest number that cannot be expressed
as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

 */
object p0023 {

  val abundants: Stream[Int] = 12 #:: Stream.from(13).filter(isAbundant)

  def properDiv(n: Int): List[Int] = {
    val max = floor(sqrt(n)).toInt
    1 :: (2 to max).filter(n % _ == 0).map { x =>
      val y = n / x
      if (x == y) List(x) else List(x, y)
    }.flatMap(x => x).toList
  }

  def isAbundant(n: Int): Boolean = properDiv(n).sum > n

  def isNotSumOf2Abundants(n: Int): Boolean =
    (for {
      x <- abundants.takeWhile(_ < n)
      if isAbundant(n - x)
    } yield n).isEmpty

  (1 to 28123).filter(isNotSumOf2Abundants).sum // 4179871
}
