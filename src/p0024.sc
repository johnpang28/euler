/*

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the
digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it
lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

 */

object p0024 {

  case class Group(left: Vector[Int], pivot: Option[Int], right: Vector[Int])

  def next(xs: Vector[Int]): Vector[Int] = {
    val g = group(xs)
    g.pivot match {
      case Some(p) =>
        val newPivot = g.right.filter(_ > p).min
        val index = g.right.lastIndexOf(newPivot)
        val newRight = g.right.updated(index, p).reverse
        g.left ++ (newPivot +: newRight)
      case None => xs
    }
  }

  def group(xs: Vector[Int]): Group = {

    def groupIter(g: Group): Group = g.pivot match {
      case Some(p) =>
        if (p < g.right.head) g else groupIter(Group(g.left.take(g.left.length - 1), Some(g.left.last), p +: g.right))
      case None => g
    }

    groupIter(Group(xs.take(xs.length - 2), Some(xs(xs.length - 2)), Vector(xs.last)))
  }

  (1 to 999999).foldLeft(Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) { (b, _) => next(b) } // Vector(2, 7, 8, 3, 9, 1, 5, 4, 6, 0)
}