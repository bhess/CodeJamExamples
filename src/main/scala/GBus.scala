/**
 * Created by i065873 on 14/11/14.
 * https://code.google.com/codejam/contest/6214486/dashboard#s=p1
 */
object GBus extends App {
  (1 to readLine().toInt).foreach(i => {
    readLine()
    def mc(in: List[Int]): List[(Int, Int)] = in match {
      case x :: y :: Nil => List((x, y))
      case x :: y :: xs => (x, y) :: mc(xs)
    }
    val intervals = mc(readLine().split(" ").map(_.toInt).toList)
    val cities = (1 to readLine().toInt).map(_ => readLine().toInt)
    val out = cities.map(x => intervals.count { case (min, max) => x >= min && x <= max}).mkString(" ")
    println(s"Case #$i: $out")
    readLine()
  })
}
