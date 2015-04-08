/**
 * Created by bhess on 08/04/15.
 * https://code.google.com/codejam/contest/1460488/dashboard#s=p2
 */
object RecycledNumbers extends App {

  def numRecycledFor(i: Int, upperBound: Int) = {
    val size = i.toString.length
    val dbl = i.toString + i.toString
    (1 to size).map(x => dbl.substring(x, x + size).toInt).distinct.count(x => x > i && x <= upperBound)
  }

  def numRecycled(a: Int, b: Int) =
    (a to b).
      toStream.
      foldLeft(0)((left, current) => left + numRecycledFor(current, b))

  (1 to readInt()).foreach(i => {
    val Array(a, b) = readLine().split(" ").map(_.toInt)
    printf(s"Case #$i: ${numRecycled(a, b)}\n")
  })

}
