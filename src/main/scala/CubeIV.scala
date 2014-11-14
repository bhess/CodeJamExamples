/**
 * Created by i065873 on 13/11/14.
 * https://code.google.com/codejam/contest/6214486/dashboard
 */
object CubeIV extends App {
  var maxVisitsFrom: Array[Int] = Array()
  var rows: Array[Array[Int]] = Array()
  def numTest = {
    val res = readLine().toInt; readLine(); res
  }
  def parseSquare: Array[Int] = readLine().split(" ").map(_.toInt)
  def maxVisits(roomStartCurrent: (Int, Int), s: Int): Int = {
    if (roomStartCurrent._1 == roomStartCurrent._2) {}
    val i = roomStartCurrent._2 / s
    val j = roomStartCurrent._2 % s
    val currentVal = rows.apply(i).apply(j)
    val neighborRooms = {
      val neighborRoomsArr = Array((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)).flatMap(k => if (k._1 >= 0 && k._1 < s && k._2 >= 0 && k._2 < s && rows.apply(k._1).apply(k._2) - 1 == currentVal) Some(k) else None).map { case (x, y) => x * s + y }
      if (neighborRoomsArr.isEmpty) None else Some(neighborRoomsArr(0))
    }
    val maxV = roomStartCurrent match {
      case (_, roomCurrent) if (maxVisitsFrom(roomCurrent) != -1) => maxVisitsFrom(roomCurrent)
      case (roomStart, roomCurrent) if (!neighborRooms.isEmpty) => 1 + maxVisits((roomStart, neighborRooms.get), s)
      case _ => 1
    }
    maxVisitsFrom(roomStartCurrent._2) = maxV
    maxV
  }

  (1 to numTest).foreach(i => {
    val j = readLine().toInt
    rows = (1 to j).toArray.map(_ => parseSquare)
    def rowVal(abs: Int) = rows.apply(abs / j).apply(abs % j)
    maxVisitsFrom = (0 to j*j-1).map(_ => -1).toArray
    val maxItem = (0 to j*j-1).map(k => (k, maxVisits((k, k), j))).foldLeft((-1, -1))((left, current) => if (current._2 > left._2) current else if (current._2 == left._2 && rowVal(current._1) < rowVal(left._1)) current else left)
    val person = rowVal(maxItem._1)
    val moves = maxItem._2
    println(s"Case #$i: $person $moves")
  })
}
