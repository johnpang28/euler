/*

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

*/
object p0005 {

  def smallestNumberDivisibleBy(r: Range): Int = {

    def process(r: Range, x: Int): Int =
      if (r.forall(x % _ == 0)) x
      else process(r, x + 1)

    process(r, r.max)
  }

  smallestNumberDivisibleBy(20 to 1 by -1) // 232792560
}