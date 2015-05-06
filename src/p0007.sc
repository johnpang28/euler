/*

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?

 */
object p0007 {

  val knownPrimes = List(2, 3, 5, 7, 11, 13)

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

  def addNextPrimeToList(ps: List[Int]): List[Int] =
    ps ::: List(nextPrime(ps.last + 1))

  def nextPrime(x: Int): Int =
    if (isPrime(x)) x
    else nextPrime(x + 1)

  def expandPrimes(max: Int, ps: List[Int]): List[Int] =
    if (ps.size == max) ps
    else expandPrimes(max, addNextPrimeToList(ps))

  def findPrimes(max: Int): List[Int] =
    expandPrimes(max, knownPrimes)

  findPrimes(10001).last
}