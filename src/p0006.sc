/*

The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is
3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

 */
object p0006 {

  def sumOfSquares(r: Range) = r.map(x => x * x).sum

  def squareOfSum(r: Range) = {
    val s = r.sum
    s * s
  }

  def diff(r: Range) = squareOfSum(r) - sumOfSquares(r)

  diff(1 to 100) // 25164150
}