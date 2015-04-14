/**
 * Created by bhess on 14/04/15.
 * https://code.google.com/codejam/contest/90101/dashboard#s=p2
 */
object WelcomeToCodeJam extends App {
  val wtcj = "welcome to code jam"
  (1 to readInt()).foreach(i => {
    val line = readLine()
    val initialArr = (1 to line.length).map(_ => 1).toArray
    val out = wtcj.zipWithIndex.foldLeft(initialArr) {
      case (prevLenArr, (currChar, currCharPos)) =>
        (0 to prevLenArr.length - 1).scanLeft(0) {
          case (_, currIndex) if (currIndex < currCharPos) =>
            0
          case (prevValue, currIndex) if (currChar == line(currIndex)) =>
            (prevValue + prevLenArr(currIndex)) % 10000
          case (prevValue, _) =>
            prevValue
        }.tail.toArray
    }
    println(s"Case #$i: ${"%04d".format(out.last)}")
  })
}
