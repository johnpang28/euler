import java.math.MathContext

/*

A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with
denominators 2 to 10 are given:

1/2 = 0.5
1/3 = 0.(3)
1/4 = 0.25
1/5 = 0.2
1/6 = 0.1(6)
1/7 = 0.(142857)
1/8 = 0.125
1/9 = 0.(1)
1/10 = 0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit
recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

 */
object p0026 {

  val mc = new MathContext(5000)
  val one = BigDecimal(1, mc)
  val maxKnownRecurring = 6

  def recurrance(s: String): Option[String] = {
    val minGroup = s.takeRight(maxKnownRecurring)
    val split = s.split(minGroup)
    if (split.size > 1 && split.tail.forall(_.equals(split.last))) Some(split.last + minGroup)
    else None
  }

  (11 until 1000).map { i =>
    val s = (one / BigDecimal(i, mc)).toString()
    (i, recurrance(s.take(4000)))
  }.filter(x => x._2.isDefined).sortBy(_._2.get.length).last._1 // d=983 (which has a 982 digit recurring cycle)
}