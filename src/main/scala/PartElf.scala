/**
 * Created by i065873 on 09/07/14.
 */
import scala.math._
object PartElf {
  case class Frac(p: BigInt, q: BigInt)
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)
  def reduceToLowestTerms(p: BigInt, q: BigInt): Frac = {
    val g = gcd(p, q)
    Frac(p / g, q / g)
  }
  def isMult2(n: BigInt): Boolean = (n & (n - 1)) == 0
  def floorToPow2(in: BigInt) = 1 << logb2(in)
  def logb2(i: BigInt): Int = {
    var ivar: BigInt = i
    var out = 0
    while ((ivar >> 1) != 0) {
      ivar >>= 1
      out = out + 1
    }
    out
  }
  def ancestorsToElf(frac: Frac) = logb2(frac.q / floorToPow2(frac.p))
  def main(args: Array[String]) {
    (1 to readLine().toInt).foreach(i => {
      val fracStr = readLine().split("/")
      val frac = reduceToLowestTerms(BigInt(fracStr(0)), BigInt(fracStr(1)))
      if (!isMult2(frac.q)) printf("Case #%d: impossible\n", i)
      else printf("Case #%d: %d\n", i, ancestorsToElf(frac))
    })
  }
}
