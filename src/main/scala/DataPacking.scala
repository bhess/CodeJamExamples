/**
 * Created by i065873 on 09/07/14.
 * https://code.google.com/codejam/contest/3014486/dashboard
 */
object DataPacking {
  def main(args: Array[String]) {
    (1 to readLine().toInt).foreach(i => {
      val bin = readLine().split(" ")(1).toInt
      val discs = readLine().split(" ").map(_.toInt).sorted
      val binsNeeded = if (discs.size == 1) 1 else discs.foldLeft((0, 0, discs.size - 1))((left, current) => {
        def bestFit(upperIndex: Int, skips: Int): (Int, Int) = {
          if (upperIndex == left._2) (upperIndex, skips)
          else if (current + discs(upperIndex) <= bin) (upperIndex, skips)
          else bestFit(upperIndex - 1, skips + 1)
        }
        if (left._2 >= left._3) left
        else {
          val bf = bestFit(left._3, 0)
          val addone = if (left._2 + 1 == bf._1 - 1) 1 else 0
          (left._1 + bf._2 + 1 + addone, left._2 + 1, bf._1 - 1)
        }
      })._1
      printf("Case #%d: %d\n", i, binsNeeded)
    })
  }
}
