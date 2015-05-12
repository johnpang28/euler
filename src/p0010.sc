/*

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.
 */
import math.sqrt

object p0010 {

  val primes: Stream[Int] = 2 #:: 3 #:: 5 #:: 7 #:: Stream.from(11).filter(isPrime)

  def isPrime(x: Int): Boolean =
    if (x < 1) false
    else if (x > 2 && x % 2 == 0) false
    else {
      val max = sqrt(x).ceil.toInt
      primes.takeWhile(_ <= max).forall(x % _ != 0)
    }

  primes.takeWhile(_ < 2000000).foldLeft(0L)(_+_) // 142913828922
}