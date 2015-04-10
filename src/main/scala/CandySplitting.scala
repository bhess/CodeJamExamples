/**
 * Created by bhess on 10/04/15.
 * https://code.google.com/codejam/contest/975485/dashboard#s=p2
 */
object CandySplitting extends App {
  (1 to readInt()).foreach(i => {
    readLine()
    val candies = readLine().split(" ").map(_.toInt)
    val out = if (candies.reduce(_ ^ _) != 0) "NO" else candies.sum - candies.min
    println(s"Case #$i: $out")
  })
}
