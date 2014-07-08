import org.saddle._

/**
 * Created by i065873 on 08/07/14.
 */
object TheRepeater {
  def removeDuplicates(in: String): String = in.foldRight(("",'$'))((right: Char, current: (String, Char)) => if (right == current._2) (current._1, right) else (right + current._1, right))._1
  def matchable(strings: Seq[String]): Int = {
    val rd = strings.map(removeDuplicates(_)).toSet
    if (rd.size == 1) rd.head.size else -1
  }
  def main(args: Array[String]) {
    val n = readLine().toInt
    (1 to n).foreach(i => {
      val m = readLine().toInt
      val strings = (1 to m).map(x => readLine())
      val chars = matchable(strings)
      if (chars == -1) printf("Case #%d: Fegla Won\n", i)
      else {
        val manipulations = (1 to chars).foldLeft((0, strings))((left, _) => {
          val reptail = left._2.map(y => {
            val repeats = y.takeWhile(_ == y.head).size
            val tail = y.drop(repeats)
            (repeats, tail)
          })
          val repeats = reptail.map(_._1)
          val tails = reptail.map(_._2)
          val medianRepeats = repeats.toVec.median.toInt
          val manipulationsNeeded = repeats.foldLeft(0)((l, c) => l + Math.abs(c - medianRepeats))
          (left._1 + manipulationsNeeded, tails)
        })._1
        printf("Case #%d: %d\n", i, manipulations)
      }
    })
  }
}
