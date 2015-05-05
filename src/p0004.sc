/*

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is
9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

 */
object p0004 {

  class Product(val x:Int, val y:Int, val p:Int)

  def isPalindrome(n: Int): Boolean = {
    val s = n.toString
    n.toString.equals(s.reverse)
  }

  def findPalindromeProducts(max: Int): Seq[Product] =
    for {
      x <- 0 to max
      y <- 0 to x
      p = x * y
      if isPalindrome(p)
    } yield new Product(x, y, p)

  def findLargestPalindromeProduct(max: Int) =
    findPalindromeProducts(max).sortBy(_.p).last.p

  findLargestPalindromeProduct(99) // 9009
  findLargestPalindromeProduct(999) // 906609
}