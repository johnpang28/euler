import scala.io.Source

/*

Name scores

Using https://projecteuler.net/project/resources/p022_names.txt, a 46K text file containing over five-thousand
first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name,
multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53,
is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?

 */
object p0022 {

  val nameFile = "https://projecteuler.net/project/resources/p022_names.txt"
  val names = Source.fromURL(nameFile).mkString.split(',').map(_.replaceAll("\"", "")).toList.sorted
  val letterScoreMap = ('A' to 'Z').zip(1 to 26).toMap

  def score(s: String) = s.toCharArray.map(letterScoreMap(_)).sum

  names.indices.map(i => score(names(i)) * (i + 1)).sum // 871198282
}
