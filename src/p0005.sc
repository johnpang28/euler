/*

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

*/
object p0005 {

  def smallestNumberDivisibleBy(r: Range): Int =
    smallestNumberDivisibleByIter(r, r.max)

  def smallestNumberDivisibleByIter(r: Range, x: Int): Int =
    if (r.forall(x % _ == 0)) x
    else smallestNumberDivisibleByIter(r, x + 1)

  smallestNumberDivisibleBy(10 to 1 by -1) // 2520
  smallestNumberDivisibleBy(20 to 1 by -1) // 232792560
}