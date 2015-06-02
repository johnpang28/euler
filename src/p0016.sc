/*

2 to the power 15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2 to the power 1000?

 */
object p0016 {

  // Could have used BigInt to do this easily, but that misses the point of this exercise!

  def power(n: String, power: Int): String  =
    if (power == 0) "0"
    else if (power == 1) n
    else (2 to power).foldLeft(n) { (b, a) => multiply(b, n.toInt) }

  def multiply(n: String, m: Int): String =
    flatten(n.toCharArray.reverse.toList.map(_.asDigit * m), Nil)._2.foldRight("") { (a, b) => a + b }

  def flatten(toFrom: (List[Int], List[Int])): (List[Int], List[Int]) = toFrom._1 match {
    case List(x) => (Nil, x :: toFrom._2)
    case x :: xs =>
      val r = x % 10
      val c = (x - r) / 10
      flatten((xs.head + c :: xs.tail, r :: toFrom._2))
    case Nil => toFrom
  }

  power("2", 1000).toCharArray.map(_.asDigit).sum // 1366
}