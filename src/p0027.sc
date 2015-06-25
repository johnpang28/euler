import scala.math._

/*

Euler discovered the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However,
when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41,
41² + 41 + 41 is clearly divisible by 41.

The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive
values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n² + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n

e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum
number of primes for consecutive values of n, starting with n = 0.

 */
object p0027 {

  case class Quad(a: Int, b: Int) {
    def maxCons(): Int = {
      def maxConsIter(n: Int):Int = if (isPrime(n*n + a*n + b)) maxConsIter(n + 1) else n - 1
      maxConsIter(0)
    }
  }

  val primes: Stream[Int] = 2 #:: 3 #:: 5 #:: 7 #:: Stream.from(11).filter(isPrime)

  def isPrime(x: Int): Boolean =
    if (x < 1 || (x > 2 && x % 2 == 0)) false
    else primes.takeWhile(_ <= sqrt(x).ceil.toInt).forall(x % _ != 0)

  val max = (for {
    a <- -999 to 999
    b <- -999 to 999
  } yield (a, b, Quad(a, b).maxCons())).sortBy(_._3).last  // a=-61, b=971, max n=70

  max._1 * max._2 // -59231
}