import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WUnDiEdge

/**
 * Created by i065873 on 21/11/14.
 * https://code.google.com/codejam/contest/5214486/dashboard#s=p1
 */
object TakingMetro extends App {

  case class Station(val line: Int, val station: Int, val in: Boolean)

  (1 to readInt).foreach(tcase => { readLine
    val stations = (1 to readInt).
      foldLeft(List[WUnDiEdge[Station]]())( (ll, mline) => {
        val (stations, waiting) = readLine.
          split(" ").
          map(_.toInt) match { case Array(i, j) => (i, j) }
        val waitingEdgesIn = (1 to stations).
          map(s => Station(mline, s, false) ~> Station(mline, s, true) % waiting).
          toList
        val waitingEdgesOut = (1 to stations).
          map(s => Station(mline, s, true) ~> Station(mline, s, false) % 0).
          toList
        val edges = readLine.
          split(" ").
          map(_.toInt).
          zip(1 to stations - 1).
          map { case (dur, st) => Station(mline, st, true) ~ Station(mline, st + 1, true) % dur}.
          toList
        ll ++ waitingEdgesIn ++ waitingEdgesOut ++ edges.
          toList
    })

    val tunnels: List[WUnDiEdge[Station]] = (1 to readInt).
      foldLeft(List[WUnDiEdge[Station]]()) ( (ll, _) => {
        val conn = readLine.
          split(" ").
          map(_.toInt) match { case Array(m1, s1, m2, s2, d) => Station(m1, s1, false) ~ Station(m2, s2, false) % d}
          conn :: ll
      }).
      groupBy(e => (e._1, e._2)).
      mapValues(_.reduceLeft((left, current) => if (left.weight < current.weight) left else current)).
      values.
      toList

    val graph = Graph.from(List(), stations ++ tunnels)
    println(s"Case #$tcase:")

    (1 to readInt).foreach(i => {
      val (from, to) = readLine.
        split(" ").
        map(_.toInt) match { case Array(m1, s1, m2, s2) => (Station(m1, s1, false), Station(m2, s2, false)) }
      graph.
        get(from).
        shortestPathTo(graph.get(to)) match {
          case None => println(-1)
          case Some(path) => println(path.weight)
        }
    })
  })
}
