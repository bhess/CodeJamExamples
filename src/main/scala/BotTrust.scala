/**
 * Created by bhess on 10/04/15.
 * https://code.google.com/codejam/contest/975485/dashboard
 */
object BotTrust extends App {
  (1 to readInt()).foreach(i => {
    val moves = readLine().split(" ").toList.tail
    val movesZipped = moves.
      zip(moves.tail).
      filter(c => Character.isDigit(c._2.head)).
      map(x => (x._1, x._2.toInt))
    val totTimeExp = movesZipped.foldLeft((1, 0, 1, 0, 0)) {
      case ((lastO, timeSinceLastO, lastB, timeSinceLastB, timeExp), (robot, button)) =>
        if (robot == "B") {
          val deltaTime = Math.max(1, 1 + Math.abs(lastB - button) - timeSinceLastB)
          (lastO, timeSinceLastO + deltaTime, button, 0, timeExp + deltaTime)
        } else {
          val deltaTime = Math.max(1, 1 + Math.abs(lastO - button) - timeSinceLastO)
          (button, 0, lastB, timeSinceLastB + deltaTime, timeExp + deltaTime)
        }
    }._5
    println(s"Case #$i: $totTimeExp")
  })
}
