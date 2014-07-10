import scala.collection.mutable

/**
 * Created by i065873 on 10/07/14.
 * https://code.google.com/codejam/contest/32013/dashboard
 */
object SavingTheUniverse {
  def main(args: Array[String]) {
    (1 to readLine().toInt).foreach(i => {
      val nrSe = readLine().toInt
      (1 to nrSe).foreach(_ => readLine())
      val seSet: mutable.HashSet[String] = new mutable.HashSet[String]()
      val nrQueries = readLine().toInt
      var nrSwitches = 0
      (1 to nrQueries).foreach(_ => {
        val query = readLine()
        seSet.add(query)
        if (seSet.size == nrSe) {
          seSet.clear
          seSet.add(query)
          nrSwitches = nrSwitches + 1
        }

      })
      printf("Case #%d: %d\n", i, nrSwitches)
    })
  }
}
