/*
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
*/
import math.sqrt

object p0003 {

  def isPrime(x: Long): Boolean =
    if (x < 1) false
    else Iterator.iterate(2L)(_ + 1).takeWhile(_ <= sqrt(x)).forall(x % _ != 0)

  def largestPrimeFactor(x: Long): Long = {

    def process(x: Long, i: Long): Long =
      if (x % i == 0) {
        val c = x / i
        if (isPrime(c)) c else process(x, i + 1)
      } else process(x, i + 1)

    process(x, 2)
  }

  largestPrimeFactor(600851475143L) // 6857
}