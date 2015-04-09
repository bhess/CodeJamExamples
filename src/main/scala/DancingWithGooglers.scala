/**
 * Created by bhess on 09/04/15.
 * https://code.google.com/codejam/contest/1460488/dashboard#s=p1
 */
object DancingWithGooglers extends App {
  (1 to readInt()).foreach(i => {
    val (n :: s :: p :: xs) = readLine().split(" ").map(_.toInt).toList
    val categories = xs.map(j => {
      (j / 3.0).ceil.toInt match {
        case k if (k == 0 && p != 0) => 0
        case k if (k >= p) => 1
        case k if (k + 1 >= p && 2*(k - 1) + (k + 1) <= j) => 2
        case _ => 0
      }
    })
    val res = categories.count(_ == 1) + Math.min(s, categories.count(_ == 2))
    printf(s"Case #$i: $res\n")
  })
}
