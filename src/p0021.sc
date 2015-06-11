import math.floor
import math.sqrt

/*

Amicable numbers

Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called
amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

 */
object p0021 {

  def d(n: Int): Int = {
    val max = floor(sqrt(n)).toInt
    (2 to max).filter(n % _ == 0).map { x =>
      val y = n / x
      if (x == y) x else x + y
    }.sum + 1
  }

  (1 to 10000).map { a =>
    val b = d(a)
    if (d(b) == a && a < b) (a, b) else (0, 0)
  }.foldLeft(0) { (d, c) => c._1 + c._2 + d } // 31626

}
