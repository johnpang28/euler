/*

Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

 */
object p0028 {

  case class Square(size: Int, corners: List[Int])

  def grow(s: Square): Square = {
    val increment = s.size + 1
    val maxDiag = s.corners.max
    Square(s.size + 2, (1 to 4).map(_ * increment + maxDiag).toList)
  }

  val squareStream: Stream[Square] = Square(1, List(1)) #:: Stream.from(1).map(i => grow(squareStream(i - 1)))

  squareStream.takeWhile(_.size <= 1001).flatMap(_.corners).sum // 669171001
}
