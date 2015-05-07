/*

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is
9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

 */
object p0004 {

  def isPalindrome(n: Int): Boolean = {
    val s = n.toString
    s.equals(s.reverse)
  }

  def findPalindromeProducts(max: Int): Seq[Int] =
    for {
      x <- 0 to max
      y <- 0 to x
      p = x * y
      if isPalindrome(p)
    } yield p

  findPalindromeProducts(999).sorted.last // 906609
}