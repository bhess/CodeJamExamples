/**
 * Created by bhess on 11/02/15.
 * Solves http://www.cryptography.com/puzzle2014
 */
object PyramidTour {

  def main(args: Array[String]): Unit = {
    val solution = solve
    solution.foreach(i => {
      printmap(i)
      println
    })
  }

  def printmap(map: Map[Field, (Piece, Orientation)]): Unit = {
    boardlist.foreach {
      case (i, j) =>
        val (piece, orientation) = map( Field(i, j) )
        print(s"(${piece.toString},${orientation.toString}) ")
        if (j == 3) println
    }
  }

  def solve = {
    val initialField = Field(0, 0)
    val (initialPiece, initialOrientation) = board(initialField).head

    val initialEmptyFields: Set[Field] = (boardlist.toSet - ((0,0))).map {
      case (i, j) => Field(i, j)
    }
    val initialPlacedFields: List[(Field, Piece, Orientation)] = List((Field(0,0), initialPiece, initialOrientation))
    val initialAvailableOptions: List[Set[(Piece, Orientation)]] = List(board(initialField) - ((initialPiece, initialOrientation)))

    val initialAvailablePieces: Map[Piece, Int] = Map(
      One() -> 1,
      Two() -> 8,
      Three() -> 7
    ).map {
      case (i, j) if (i == initialPiece) => (initialPiece, j - 1)
      case (i, j) => (i, j)
    }

    pyramidTour(initialEmptyFields, initialPlacedFields, initialAvailableOptions, initialAvailablePieces, false, Set(List())).
      map(_.map(i => i._1 -> (i._2, i._3)).toMap)
  }

  case class Field(i: Int, j: Int) {
    def isValid(p: Piece, o: Orientation): Boolean = (p, o) match {
      case (Three(), Up())    => i == 3
      case (Three(), Right()) => j == 0
      case (Three(), Left())  => j == 3
      case (Three(), Down())  => i == 0
      case (Two(),   Up())    => i >= 2
      case (Two(),   Right()) => j <= 1
      case (Two(),   Left())  => j >= 2
      case (Two(),   Down())  => i <= 1
      case (One(),   Up())    => i != 0
      case (One(),   Right()) => j != 3
      case (One(),   Left())  => j != 0
      case (One(),   Down())  => i != 3
    }
  }

  trait Piece
  case class Three() extends Piece {
    override def toString = "3"
  }
  case class Two() extends Piece {
    override def toString = "2"
  }
  case class One() extends Piece {
    override def toString = "1"
  }

  trait Orientation
  case class Up()    extends Orientation {
    override def toString = "U"
  }
  case class Right() extends Orientation {
    override def toString = "R"
  }
  case class Down()  extends Orientation {
    override def toString = "D"
  }
  case class Left()  extends Orientation {
    override def toString = "L"
  }

  val boardlist = Vector(
    (0, 0), (0, 1), (0, 2), (0, 3),
    (1, 0), (1, 1), (1, 2), (1, 3),
    (2, 0), (2, 1), (2, 2), (2, 3),
    (3, 0), (3, 1), (3, 2), (3, 3)
  )
  val pieces: Vector[(Piece, Orientation)] = Vector(
    (Three(), Up())   ,
    (Three(), Right()),
    (Three(), Left()) ,
    (Three(), Down()) ,
    (Two(),   Up())   ,
    (Two(),   Right()),
    (Two(),   Left()) ,
    (Two(),   Down()) ,
    (One(),   Up())   ,
    (One(),   Right()),
    (One(),   Left()) ,
    (One(),   Down())
  )

  val board: Map[Field, Set[(Piece, Orientation)]] = boardlist.map(i => Field(i._1, i._2)).map(i => (i -> pieces.filter(j => i.isValid(j._1, j._2)).toSet)).toMap

  def next(f: Field, p: Piece, o: Orientation): Field = (p, o) match {
    case (Three(), Up())    => Field(f.i - 3, f.j    )
    case (Three(), Right()) => Field(f.i    , f.j + 3)
    case (Three(), Left())  => Field(f.i    , f.j - 3)
    case (Three(), Down())  => Field(f.i + 3, f.j    )
    case (Two()  , Up())    => Field(f.i - 2, f.j    )
    case (Two()  , Right()) => Field(f.i    , f.j + 2)
    case (Two()  , Left())  => Field(f.i    , f.j - 2)
    case (Two()  , Down())  => Field(f.i + 2, f.j    )
    case (One()  , Up())    => Field(f.i - 1, f.j    )
    case (One()  , Right()) => Field(f.i    , f.j + 1)
    case (One()  , Left())  => Field(f.i    , f.j - 1)
    case (One()  , Down())  => Field(f.i + 1, f.j    )
  }

  /**
   *
   * @param emptyFields: fields that are still available. if none is available, the solution is found
   * @param placedFields pieces in specified orientations placed on some fields (actually a Stack, but in scala a List)
   * @param availableOptions Stack of available options remaining in the current path
   * @param availablePieces available pieces and their number...
   * @return
   *
   *         cases to check
   *         1) next field is     taken and is     the upper left corner, all fields are taken
   *         2) next field is     taken
   *         3) next field is not taken and there is    valid piece available for the next field
   *         4) next field is not taken and there is no valid piece available for the next field
   *
   *         what to do...
   *         1) -> Success, return solution
   *         2) -> another piece is available for the current position?
   *              2.1) Yes -> remove current piece from available options, place new one to current field, recursion on current field
   *              2.2) No  -> add current piece to empty fields, recursion on previous field (pop stacks)
   *         3) -> add piece to placed fields, remove next field from empty fields, recursion on next field
   *         4) -> another piece is available for the current position?
   *              4.1) Yes -> remove current piece from available options, place new one to current field, recursion on current field
   *              4.2) No  -> add current piece to empty fields, recursion on previous field (pop stacks)
   *
   *
   *
   */
  def pyramidTour(emptyFields: Set[Field],
            placedFields: List[(Field, Piece, Orientation)],
            availableOptions: List[Set[(Piece, Orientation)]],
            availablePieces: Map[Piece, Int],
            lastCaseException: Boolean, solutions: Set[List[(Field, Piece, Orientation)]]): Set[List[(Field, Piece, Orientation)]] = {


    def validPlacementForNextField(nextField: Field): Option[(Piece, Orientation)] = {
      availablePieces.
        filter(_._2 > 0).
        keySet.
        flatMap(x => Set((x, Left()), (x, Right()), (x, Up()), (x, Down()))).
        toSet.
        intersect(board(nextField)).
        headOption
    }

    def availableOptionsAt(f: Field, oneAvail: Boolean, twoAvail: Boolean, threeAvail: Boolean): Set[(Piece, Orientation)] = {
      Set(
        (if (oneAvail) Some(One()) else None),
        (if (twoAvail) Some(Two()) else None),
        (if (threeAvail) Some(Three()) else None)
      ).
        flatten.
        flatMap(x => Set((x, Up()), (x, Right()), (x, Left()), (x, Down()))).
        toSet.
        intersect(board(f))
    }
    def availablePiecesNext(without: Piece): Map[Piece, Int] = availablePieces.map {
      case (i, j) if (i == without) => (without, j - 1)
      case (i, j)                   => (i, j)
    }

    def case21(currentFieldNextPiece: Piece, currentFieldNextOrientation: Orientation, currentField: Field) = case41(currentFieldNextPiece, currentFieldNextOrientation, currentField)

    def case22(currentField: Field) = case42(currentField)

    def case41(currentFieldNextPiece: Piece, currentFieldNextOrientation: Orientation, currentField: Field) = {
      val placedFieldsNext = (currentField, currentFieldNextPiece, currentFieldNextOrientation) :: placedFields.tail
      val availableOptionsNext = (availableOptions.head - ((currentFieldNextPiece, currentFieldNextOrientation))) :: availableOptions.tail
      val currentPiece = placedFields.head._2
      val availablePiecesNext = if (currentPiece == currentFieldNextPiece) availablePieces else availablePieces.map {
        case (j, i) if (j == currentPiece) => (currentPiece, i + 1)
        case (j, i) if (j == currentFieldNextPiece) => (currentFieldNextPiece, i - 1)
        case (j, i) => (j, i)
      }
      (emptyFields, placedFieldsNext, availableOptionsNext, availablePiecesNext, false)
    }

    def case42(currentField: Field) = {
      //println("case4.2")

      val emptyFieldsNext = emptyFields + currentField
      val placedFieldsNext = placedFields.tail
      val availableOptionsNext = availableOptions.tail
      val currentPiece = placedFields.head._2
      val availablePiecesNext = availablePieces.map {
        case (j, i) if (j == currentPiece) => (currentPiece, i + 1)
        case (j, i) => (j, i)
      }
      (emptyFieldsNext, placedFieldsNext, availableOptionsNext, availablePiecesNext, true)
    }

    def case3(nextField: Field, nextPiece: Piece, nextOrientation: Orientation) = {
      val currentAvailableOptions = availableOptions.head
      val emptyFieldsNext = emptyFields - nextField
      val placedFieldsNext = (nextField, nextPiece, nextOrientation) :: placedFields
      val availablePiecesNxt = availablePiecesNext(nextPiece)

      val availableOptionsNext = availableOptionsAt(
        nextField,
        (nextPiece == One() || availablePiecesNxt(One()) > 0),
        (nextPiece == Two() || availablePiecesNxt(Two()) > 0),
        (nextPiece == Three() || availablePiecesNxt(Three()) > 0)) :: availableOptions
      (emptyFieldsNext, placedFieldsNext, availableOptionsNext, availablePiecesNxt, false)
    }

    placedFields match {
      case (Field(i, j), p, o) :: xs =>
        val sol = if (emptyFields.size == 0 && next(Field(i, j), p, o) == Field(0,0)) solutions + placedFields else solutions

        val nextField = next(Field(i, j), p, o)
        if (!emptyFields.contains(nextField)) {
          val currentAvailableOptions = availableOptions.head
          val (emptyFieldsNext, placedFieldsNext, availableOptionsNext, availablePiecesNext, exception) =
            if (currentAvailableOptions.exists(x => p == x._1 || availablePieces(x._1) > 0)) {
              // case 2.1)
              val (nextPiece, nextOrientation) = currentAvailableOptions.filter(x => p == x._1 || availablePieces(x._1) > 0).head
              case21(nextPiece, nextOrientation, Field(i, j))
            } else {
              // case 2.2)
              case22(Field(i, j))
            }
          pyramidTour(emptyFieldsNext, placedFieldsNext, availableOptionsNext, availablePiecesNext, exception, sol)
        } else {
          val currentAvailableOptions = availableOptions.head
          val (emptyFieldsNext, placedFieldsNext, availableOptionsNext, availablePiecesNext, exception) =
            validPlacementForNextField(nextField) match {
              case Some((nextPiece, nextOrientation)) if !lastCaseException =>
                // case 3)
                case3(nextField, nextPiece, nextOrientation)
              case _ if currentAvailableOptions.exists(x => p == x._1 || availablePieces(x._1) > 0) =>
                // case 4.1)
                val (nextPiece, nextOrientation) = currentAvailableOptions.filter(x => p == x._1 || availablePieces(x._1) > 0).head
                case41(nextPiece, nextOrientation, Field(i, j))
              case _ =>
                // case 4.2)
                case42(Field(i, j))
            }
          pyramidTour(emptyFieldsNext, placedFieldsNext, availableOptionsNext, availablePiecesNext, exception, sol)

        }
      case _ => solutions - List()
    }
  }

}
