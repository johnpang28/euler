/*

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?

 */
object p0007 {

  val primes: Stream[Int] = Stream.from(2).filter(isPrime(_))

  def hasHigherFactor(x: Long, f: Long, max: Long): Boolean =
    if (f > max) false
    else if (x % f == 0) true
    else hasHigherFactor(x, f + 1, max)

  def isPrime(x: Long): Boolean =
    if (x < 1) false
    else {
      val max = if (x % 2 == 0) x / 2 else (x + 1) / 2
      !hasHigherFactor(x, 2, max)
    }

  primes(10000) // remember index starts at 0. answer is 104743
}