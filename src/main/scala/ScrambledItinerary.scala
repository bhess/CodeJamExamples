/**
 * Created by i065873 on 14/11/14.
 * https://code.google.com/codejam/contest/6214486/dashboard
 */
object ScrambledItinerary extends App {
  (1 to readLine().toInt).foreach(i => {
    val scrambled = (1 to readLine().toInt).map(_ => (readLine() -> readLine()).swap).toMap
    def pos(elem: (String, String), curpos: Int): Int = elem match {
      case (_, dep) if (scrambled.contains(dep)) => pos((dep, scrambled.get(dep).get), curpos + 1)
      case _ => curpos
    }
    val posscrambled = scrambled.map(j => pos(j, 0) -> j)
    val out = (0 to posscrambled.size - 1).map(j => posscrambled.get(j).get).map { case (arr, dep) => s"$dep-$arr"}.mkString(" ")
    println(s"Case #$i: $out")
  })
}
