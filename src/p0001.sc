/*

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these
multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

*/
object p0001 {

  def isMultipleOfThreeOrFive(x: Int): Boolean = x % 3 == 0 || x % 5 == 0

  def sumMultiples(ceiling: Int): Int =
    if (ceiling > 1)
      if (isMultipleOfThreeOrFive(ceiling)) ceiling + sumMultiples(ceiling - 1)
      else sumMultiples(ceiling - 1)
    else 0

  sumMultiples(999)
}