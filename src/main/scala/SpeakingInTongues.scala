/**
 * Created by bhess on 02/04/15.
 * https://code.google.com/codejam/contest/1460488/dashboard#s=p0
 */
object SpeakingInTongues extends App {

  val fromList = List(
    "ejp mysljylc kd kxveddknmc re jsicpdrysi",
    "rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd",
    "de kr kd eoya kw aej tysr re ujdr lkgc jv"
  )
  val toList = List(
    "our language is impossible to understand",
    "there are twenty six factorial possibilities",
    "so it is okay if you want to just give up"
  )
  val trainingMapping = fromList.
                    zip(toList).
                    flatMap(i => i._1 zip i._2).
                    toMap
  val allMappings = (
    (('a' to 'z').toSet &~ trainingMapping.keySet).toList.reverse zip
    (('a' to 'z').toSet &~ trainingMapping.values.toSet).toList
    ).toMap ++ trainingMapping
  (1 to readInt()).foreach(i => {
    println(s"Case #$i: ${readLine().map(allMappings(_))}")
  })
}
