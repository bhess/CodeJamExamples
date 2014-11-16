import scala.collection.immutable.Queue
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Created by i065873 on 16/11/14.
 * https://code.google.com/codejam/contest/5214486/dashboard
 */
object Minesweeper extends App {
  def printGrid(grid: Array[Array[(Char, (Int, Int))]]): Unit = {
    grid.foreach(u => {
      println(u.mkString(""))
    })
    println
  }
  def aroundQueue(i: Int, j: Int): Set[(Int, Int)] =
    Set(
    (i-1, j-1), (i-1, j), (i-1, j+1),
    (i, j-1),             (i, j+1),
    (i+1, j-1), (i+1, j), (i+1, j+1))

  (1 to readInt).foreach(x => {
    val n = readInt
    val grid = (0 to n + 1).toArray.map {
      case 0 => ArrayBuffer.fill(n + 2)('o').zipWithIndex.map(y => (y._1, (0, y._2)))
      case m if (m == n + 1) => ArrayBuffer.fill(n + 2)('o').zipWithIndex.map(y => (y._1, (n + 1, y._2)))
      case m => ArrayBuffer(('o',(m,0))) ++ readLine.map {
        case '.' => 'c'
        case '*' => 'm'
      }.toArray.zipWithIndex.map(y => (y._1, (m, y._2 + 1))) ++ ArrayBuffer(('o',(m,n + 1)))
    }
    @tailrec def resolve(toResolve: Set[(Int, Int)]): Unit = {
      if (!toResolve.isEmpty) {
        toResolve.toList match {
          case ((ei, ej) :: q1) =>
            val aroundMe = aroundQueue(ei, ej).filter(k => grid(k._1)(k._2)._1 != 'm')
            val newQ = if (aroundMe.size != 8) Set() else aroundMe.filter(k => grid(k._1)(k._2)._1 == 'c').toSet
            grid(ei)(ej) = ('u', (ei, ej))
            resolve(q1.toSet ++ newQ)
        }
      }
    }
    @tailrec def minSteps(i: Int, j: Int, pass: Int, steps: Int): Int = {
      val (newGrid, newStep) = if (grid(i)(j)._1 == 'c' && (pass > 1 || aroundQueue(i, j).count(k => grid(k._1)(k._2)._1 != 'm') == 8)) {
        (resolve(Set((i, j))), steps + 1)
      } else (grid, steps)
      val (nexti, nextj) = if (i == n && j == n) (1, 1) else if (j == n) (i + 1, 1) else (i, j + 1)
      val nextpass = if (i == n && j == n) pass + 1 else pass
      if (grid.foldLeft(0)((left, current) => left + current.count(_._1 == 'c')) == 0) {
        newStep
      } else {
        minSteps(nexti, nextj, nextpass, newStep)
      }
    }
    val stepsNeeded = minSteps(1, 1, 1, 0)
    println(s"Case #$x: $stepsNeeded")
  })
}
