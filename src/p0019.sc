/*

You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.

Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.

A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

 */
object p0019 {

  case class Date(dayOfWeek: Int, dayOfMonth: Int, month: Int, year: Int)

  val monthDayMap =
    Map(1 -> 31, 2 -> 28, 3 -> 31, 4 -> 30, 5 -> 31, 6 -> 30, 7 -> 31, 8 -> 31, 9 -> 30, 10 -> 31, 11 -> 30, 12 -> 31)

  val dates: Stream[Date] = Date(1, 1, 1, 1900) #:: Stream.from(1).map(i => nextDay(dates(i - 1)))

  def nextDay(d: Date): Date = {

    def nextDayInWeek(diw: Int): Int = if (diw == 7) 1 else diw + 1
    def nextMonth(m: Int): Int = if (m == 12) 1 else m + 1
    def isLeapYear(y: Int): Boolean = if (y % 4 == 0) !(y % 100 == 0 && y % 400 != 0) else false
    def maxDaysInMonth(year: Int, month: Int) = if (isLeapYear(year) && month == 2) 29 else monthDayMap(month)

    val daysInMonth = maxDaysInMonth(d.year, d.month)
    val day = if (d.dayOfMonth < daysInMonth) d.dayOfMonth + 1 else 1
    val month = if (day == 1) nextMonth(d.month) else d.month
    val year = if (day == 1 && month == 1) d.year + 1 else d.year
    Date(nextDayInWeek(d.dayOfWeek), day, month, year)
  }

  dates.takeWhile(d => !(d.dayOfMonth == 1 && d.month == 1 && d.year == 2001))
    .count(d => d.year != 1900 && d.dayOfMonth == 1 && d.dayOfWeek == 7) // 171
}