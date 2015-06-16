/*

1000-digit Fibonacci number

The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144

The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

 */
object p0025 {

  val fibs: Stream[String] = "1" #:: "1" #:: fibs.zip(fibs.tail).map { n => add(n._1, n._2) }

  def add(x: String, y: String): String = {

    def asString(ns: List[Int]): String = ns.foldLeft("") { (b, a) => b + a }

    def prependZero(s: String, length: Int): String = {
      def prependZeroIter(i: String): String = if (i.length == length) i else prependZeroIter("0" + i)
      prependZeroIter(s)
    }

    def flatten(a: (List[Int], List[Int])): (List[Int], List[Int]) = a._1 match {
      case List(b) => (Nil, b :: a._2)
      case b::bs =>
        val r = b % 10
        val c = (b - r) / 10
        flatten((bs.head + c :: bs.tail, r :: a._2))
    }

    if (x.length < y.length) add(y, x)
    else asString(flatten(x.zip(prependZero(y, x.length)).map(xy => xy._1.asDigit + xy._2.asDigit).toList.reverse, Nil)._2)
  }

  fibs.takeWhile(_.length < 1000).size + 1 // 4782
}
