import scala.collection.mutable.ArrayBuffer

/**
 * Created by i065873 on 14/11/14.
 * https://code.google.com/codejam/contest/6214486/dashboard#s=p3
 */
object ItzChess extends App {
  val lettermap = Map('A' -> 8, 'B' -> 7, 'C' -> 6, 'D' -> 5, 'E' -> 4, 'F' -> 3, 'G' -> 2, 'H' -> 1 )
  var board: ArrayBuffer[ArrayBuffer[Field]] = initBoard
  def initBoard: ArrayBuffer[ArrayBuffer[Field]] = {
    ArrayBuffer.tabulate(10, 10) {
      case (i, j) if (i == 0 || i == 9 || j == 0 || j == 9) => Out(i, j)
      case (i, j) => EmptyField(i, j)
    }
  }
  def killOne(in: Seq[(Int, Int)]) = in.foldLeft(0) {
      case (1, _) => 1
      case (0, (ti, tj)) => if (board(ti)(tj).isElement) 1 else 0
    }
  def getPiece(code: String, i: Int, j: Int): Field = code match {
    case "K" => King(i, j)
    case "Q" => Queen(i, j)
    case "R" => Rook(i, j)
    case "B" => Bishop(i, j)
    case "N" => Knight(i, j)
    case "P" => Pawn(i, j)
  }

  trait Field {
    val i: Int
    val j: Int
    def kill: Int
    def isElement: Boolean
  }
  trait DiagonalKiller extends Field {
    def dr = (i + 1 to 9).zip(j + 1 to 9)
    def ur = Range(i - 1, 0, -1).zip(j + 1 to 9)
    def ul = Range(i - 1, 0, -1).zip(Range(j - 1, 0, -1))
    def dl = (i + 1 to 9).zip(Range(j - 1, 0, -1))
    def killDiagonal: Int = killOne(dr) + killOne(ur) + killOne(ul) + killOne(dl)
  }
  trait VertHorKiller extends Field {
    def d = (i + 1 to 9).zip(Seq.fill(8)(j))
    def u = Range(i - 1, 0, -1).zip(Seq.fill(8)(j))
    def r = Seq.fill(8)(i).zip(j + 1 to 9)
    def l = Seq.fill(8)(i).zip(Range(j - 1, 0, -1))
    def killVertHor: Int = killOne(d) + killOne(u) + killOne(r) + killOne(l)
  }
  case class EmptyField(val i: Int, val j: Int) extends Field {
    def kill = 0
    def isElement = false
  }
  case class Out(val i: Int, val j: Int) extends Field {
    def kill = 0
    def isElement = false
  }
  case class King(val i: Int, val j: Int) extends Field {
    def reachable = List((i - 1, j - 1), (i - 1, j), (i - 1, j + 1), (i, j + 1), (i + 1, j + 1), (i + 1, j), (i + 1, j - 1), (i, j - 1))
    def kill = reachable.count { case (i, j) => board(i)(j).isElement }
    def isElement = true
  }
  case class Queen(val i: Int, val j: Int) extends Field with DiagonalKiller with VertHorKiller {
    def isElement = true
    def kill = killDiagonal + killVertHor
  }
  case class Rook(val i: Int, val j: Int) extends Field with VertHorKiller {
    def isElement = true
    def kill = killVertHor
  }
  case class Bishop(val i: Int, val j: Int) extends Field with DiagonalKiller {
    def isElement = true
    def kill = killDiagonal
  }
  case class Knight(val i: Int, val j: Int) extends Field {
    def reachable = List((i - 2, j + 1), (i - 1, j + 2), (i + 1, j + 2), (i + 2, j + 1), (i + 2, j - 1), (i + 1, j - 2), (i - 1, j - 2), (i - 2, j - 1)).filter{ case (i, j) => i > 0 && i < 9 && j > 0 && j < 9}
    def isElement = true
    def kill = reachable.count {case (i, j) => board(i)(j).isElement}
  }
  case class Pawn(val i: Int, val j: Int) extends Field {
    def isElement = true
    def reachable = List((i + 1, j - 1), (i - 1, j - 1)).filter{ case (i, j) => i > 0 && i < 9 && j > 0 && j < 9}
    def kill = reachable.count{case (i, j) => board(i)(j).isElement}
  }

  (1 to readInt).foreach(i => {
    (1 to readInt).foreach(j => {
      readLine().split("-") match {
        case Array(field, piece) =>
          val fi = field(1).toString.toInt
          val fj = lettermap.get(field(0)).get
          board(fi)(fj) = getPiece(piece, fi, fj)
      }
    })
    val numkills = board.map(_.map(_.kill)).foldLeft(0)((left, current) => left + current.sum)
    println(s"Case #$i: $numkills")
    board = initBoard
  })

}
