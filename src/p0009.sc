/*

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

 */
object p0009 {

  val candidates: Seq[(Int, Int, Int)] = for {
    a <- 1 to 332
    b <- a+1 to 499
    c = 1000 - a - b
    if (a < b) && (b < c)
  } yield (a, b, c)

  val pt = candidates.filter(x => x._1 * x._1 + x._2 * x._2 == x._3 * x._3).head // (200,375,425)

  pt._1 * pt._2 * pt._3 // 31875000
}