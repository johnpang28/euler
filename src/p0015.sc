/*

Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6
routes to the bottom right corner.

https://projecteuler.net/project/images/p015.gif

How many such routes are there through a 20×20 grid?

 */
object p0015 {

  case class Pos(x: Int, y: Int)

  def routeCount(gridWith: Int): Long = {

    def routeCountIter(current: Pos, knownCounts: Map[Pos, Long]): Map[Pos, Long] =
      if (current.x == gridWith && current.y == gridWith) knownCounts
      else {
        val next = nextIncrement(current)
        val nextCount = knownCounts(normalise(Pos(next.x - 1, next.y))) + knownCounts(normalise(Pos(next.x, next.y - 1)))
        routeCountIter(next, knownCounts + (next -> nextCount))
      }

    def nextIncrement(p: Pos): Pos =
      if (p.x == p.y) Pos(p.x + 1, 1)
      else if (p.x > p.y) Pos(p.x, p.y + 1)
      else Pos(p.x + 1, p.y)

    def normalise(p: Pos) = if (p.x >= p.y) p else Pos(p.y, p.x)

    def createKnowns(): Map[Pos, Long] = (for (x <- 1 to gridWith) yield (Pos(x, 0), 1L)).toMap + (Pos(1, 1) -> 2L)

    routeCountIter(Pos(1, 1), createKnowns())(Pos(gridWith, gridWith))
  }

  routeCount(20) // 137846528820
}
