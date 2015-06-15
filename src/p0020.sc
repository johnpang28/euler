/*

Factorial digit sum.

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!

 */
object p0020 {

  // Could have solved using BigInt, but that defeats the point of the problem, which is to handle big numbers which
  // don't fit into a regular number type.

  def fac(n: Int): String = {

    def multiply(n: String, d: Int): String = asString(flatten((n.toCharArray.map(_.asDigit * d).toList.reverse, Nil))._2)

    def asString(ns: List[Int]): String = ns.foldLeft("") { (b, a) => b + a }

    def flatten(x: (List[Int], List[Int])): (List[Int], List[Int]) = x._1 match {
      case List(y) => (Nil, y :: x._2)
      case y::ys =>
        val r = y % 10
        val c = (y - r) / 10
        flatten((ys.head + c :: ys.tail, r :: x._2))
    }

    def facIter(c: String, m: Int): String = if (m > n) c else facIter(multiply(c, m), m + 1)

    facIter("1", 2)
  }

  val fac100 = fac(100)                 // 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
  fac100.toCharArray.map(_.asDigit).sum //648
}