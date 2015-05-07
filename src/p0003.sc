/*
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
*/
object p0003 {

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

  def largetPrimeFactorIter(x: Long, i: Long): Long =
    if (x % i == 0) {
      val c = x / i
      if (isPrime(c)) c
      else largetPrimeFactorIter(x, i + 1)
    } else largetPrimeFactorIter(x, i + 1)

  def largestPrimeFactor(x: Long): Long =
    largetPrimeFactorIter(x, 2)

  largestPrimeFactor(600851475143L) // 6857
}