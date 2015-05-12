/*

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?

 */

import scala.math._

object p0007 {

  val primes: Stream[Int] = 2 #:: 3 #:: 5 #:: 7 #:: 11 #:: 13 #:: Stream.from(17).filter(isPrime)

  def isPrime(x: Int): Boolean =
    if (x < 1) false
    else if (x > 2 && x % 2 == 0) false
    else {
      val max = sqrt(x).ceil.toInt
      primes.takeWhile(_ <= max).forall(x % _ != 0)
    }

  primes(10000) // remember index starts at 0. answer is 104743
}