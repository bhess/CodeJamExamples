/**
 * Created by i065873 on 08/07/14.
 * https://code.google.com/codejam/contest/2974486/dashboard
 */
object MagicTrick {
  def main(args: Array[String]) {
    val nr = readLine().toInt
    (1 to nr).foreach(x => {
      val sol1 = readLine().toInt - 1
      val r1 = Array(
        readLine.split(" ").map(_.toInt),
        readLine.split(" ").map(_.toInt),
        readLine.split(" ").map(_.toInt),
        readLine.split(" ").map(_.toInt)
      )

      val sol2 = readLine().toInt - 1
      val r2 = Array(
        readLine.split(" ").map(_.toInt),
        readLine.split(" ").map(_.toInt),
        readLine.split(" ").map(_.toInt),
        readLine.split(" ").map(_.toInt)
      )

      val overlap = r1(sol1).flatMap(y => if (r2(sol2).contains(y)) Some(y) else None)
      println("Case #" + x + ": " + {
        if (overlap.size == 0) "Volunteer cheated!"
        else if (overlap.size == 1) overlap.head
        else "Bad magician!"
      })

    })
  }
}
