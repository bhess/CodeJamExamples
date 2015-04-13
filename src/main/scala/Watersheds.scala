import scala.annotation.tailrec

/**
 * Created by bhess on 13/04/15.
 * https://code.google.com/codejam/contest/90101/dashboard#s=p1
 */
object Watersheds extends App {

  val allChars = ('a' to 'z').toArray

  (1 to readInt()).foreach(i => {
    val Array(x, y) = readLine().split(" ").map(_.toInt)

    def isOOB(cx: Int, cy: Int) = cx < 0 || cy < 0 || cx >= x || cy >= y

    def basinAround(cx: Int, cy: Int) =
      Array((cx - 1, cy), (cx, cy - 1), (cx, cy + 1), (cx + 1, cy)).
        filter(j => !isOOB(j._1, j._2))

    val basin =
      (0 to x - 1).
        flatMap(j => readLine().
        split(" ").
        map(_.toInt).
        zipWithIndex.
        map(k => (j, k._2) -> k._1)).
        toMap

    val sinks = basin.map {
      case ((cx, cy), alt) =>
        (cx, cy) -> basinAround(cx, cy).forall(basin(_) >= alt)
    }

    def minAround(cx: Int, cy: Int): (Int, Int) =
      basinAround(cx, cy).
        reduceLeft((left, current) => if (basin(current) < basin(left)) current else left)

    @tailrec
    def watershedFor(c: (Int, Int)): (Int, Int) = c match {
      case (cx, cy) if sinks((cx, cy)) => (cx, cy)
      case (cx, cy) => watershedFor(minAround(cx, cy))
    }

    val watersheds =
      basin.
        map(j => j._1 -> watershedFor(j._1))

    val charForWatersheds =
      watersheds.
        toList.
        groupBy(_._2).
        mapValues(_.min).
        toArray.
        sortBy(_._2._1).
        map(_._1).
        zipWithIndex.map(j => j._1 -> allChars(j._2)).
        toMap

    println(s"Case #$i:")
    (0 to x-1).foreach(ci => {
      println( (0 to y-1).map(cj => charForWatersheds(watersheds(ci, cj))).mkString(" ") )
    })

  })

}
