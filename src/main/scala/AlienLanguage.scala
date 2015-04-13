/**
 * Created by bhess on 13/04/15.
 * https://code.google.com/codejam/contest/90101/dashboard
 */
object AlienLanguage extends App {
  val Array(l, d, n) = readLine().split(" ").map(_.toInt)
  val vocab = (1 to d).map(_ => readLine())
  (1 to n).foreach(i => {
    val regex = readLine().replace('(', '[').replace(')', ']')
    println(s"Case #$i: ${vocab.count(_.matches(regex))}")
  })
}
