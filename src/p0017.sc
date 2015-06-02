/*

If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19
letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one
hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

 */
object p0017 {

  case class EnglishNumber(x1000: Option[Int], x100: Option[Int], lt100ge20: Option[Int], lt20: Option[Int])

  val numberMap = Map(
    1 -> "one", 2 -> "two", 3 -> "three",  4 -> "four",  5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight",
    9 -> "nine", 10 -> "ten", 11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen",
    16 -> "sixteen", 17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen", 20 -> "twenty", 30 -> "thirty",
    40 -> "forty", 50 -> "fifty",  60 -> "sixty", 70 -> "seventy", 80 -> "eighty",  90 -> "ninety", 100 -> "hundred",
    1000  -> "thousand"
  )

  def breakdown(n: Int): EnglishNumber =
    if (n < 20) EnglishNumber(None, None, None, Some(n))
    else if (n >= 20 && n < 100) {
      val r = n % 10
      if (r == 0) EnglishNumber(None, None, Some(n), None)
      else EnglishNumber(None, None, Some(n - r), Some(r))
    } else if (n >= 100 && n < 1000) {
      val r = n % 100
      val c = n - r
      if (r == 0) EnglishNumber(None, Some(c / 100), None, None)
      else {
        val lt100 = breakdown(r)
        EnglishNumber(None, Some(c / 100), lt100.lt100ge20, lt100.lt20)
      }
    } else if (n == 1000) EnglishNumber(Some(1), None, None, None)
    else throw new Error("Number not supported" )

  def toWordsFromInt(n: Int): List[String] = toWordsFromEnglishNumber(breakdown(n))

  def toWordsFromEnglishNumber(n: EnglishNumber): List[String] = n match {
    case EnglishNumber(None, None, None, Some(lt20)) => List(numberMap(lt20))
    case EnglishNumber(None, None, Some(lt100ge20), None) => List(numberMap(lt100ge20))
    case EnglishNumber(None, None, Some(lt100ge20), Some(lt20)) => List(numberMap(lt100ge20), numberMap(lt20))
    case EnglishNumber(None, Some(x100), None, None) => List(numberMap(x100), numberMap(100))
    case EnglishNumber(None, Some(x100), Some(lt100ge20), None) => List(numberMap(x100), numberMap(100), "and", numberMap(lt100ge20))
    case EnglishNumber(None, Some(x100), Some(lt100ge20), Some(lt20)) => List(numberMap(x100), numberMap(100), "and", numberMap(lt100ge20), numberMap(lt20))
    case EnglishNumber(None, Some(x100), None, Some(lt20)) => List(numberMap(x100), numberMap(100), "and", numberMap(lt20))
    case EnglishNumber(Some(x1000), None, None, None) => List(numberMap(x1000), numberMap(1000))
  }

  (1 to 1000).foldLeft(0) { (b, a) => toWordsFromInt(a).map(_.length).sum + b } // 21124
}