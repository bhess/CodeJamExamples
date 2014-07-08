/**
 * Created by i065873 on 08/07/14.
 * https://code.google.com/codejam/contest/2984486/dashboard
 */
object ChargingChaos {
  def main(args: Array[String]) {
    val n = readLine().toInt
    (1 to n).foreach(i => {
      val firstLine = readLine().split(" ").map(_.toInt)
      val firstState = readLine().split(" ").map(BigInt(_, 2))
      val endState = readLine().split(" ").map(BigInt(_, 2))

      val minTransition = endState.map(j => {
        def allSocketsMatch(pt: Set[BigInt], es: Set[BigInt]): Boolean = pt.union(es).size == pt.size
        val pattern: BigInt = firstState.head ^ j
        val potentialTransform = firstState.map(_ ^ pattern)
        if (allSocketsMatch(potentialTransform.toSet, endState.toSet)) {
          pattern.toString(2).count(_ == '1')
        } else {
          Integer.MAX_VALUE
        }

      }).min
      print("Case #" + i + ": ")
      if (minTransition == Integer.MAX_VALUE) {
        println("NOT POSSIBLE")
      } else {
        println(minTransition)
      }
    })
  }
}
