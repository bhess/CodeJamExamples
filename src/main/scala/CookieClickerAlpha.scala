/**
 * Created by i065873 on 19/07/14.
 * https://code.google.com/codejam/contest/2974486/dashboard#s=p1
 */
object CookieClickerAlpha {
  def main(args: Array[String]) {
    (1 to readLine().toInt).foreach(i => {
      val cfx = readLine().split(" ").map(_.toDouble)
      val c = cfx(0)
      val f = cfx(1)
      val x = cfx(2)
      val out = Range(0, Integer.MAX_VALUE).toStream.map(j => {
        val currentF = 2 + j * f
        val secToFarm = c / currentF
        val secToFarmAndThen = secToFarm + (x / (currentF + f))
        val secToX = x / currentF
        (secToFarm, secToX >= secToFarmAndThen)
      }).takeWhile(_._2)
      val lastEl = out.foldLeft(x / (2 + out.size * f))((left, current) => left + current._1)
      printf("Case #%d: %f\n", i, lastEl)
    })
  }
}
