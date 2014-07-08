

/**
 * Created by i065873 on 08/07/14.
 * https://code.google.com/codejam/contest/351101/dashboard
 */
object ReverseWords {
  def main(args: Array[String]) {
    (1 to readLine().toInt).foreach(x => println("Case #" + x + ": " + readLine().split(" ").reverse.mkString(" ")))
  }

}
