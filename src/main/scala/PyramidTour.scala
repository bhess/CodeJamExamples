/**
 * Created by bhess on 11/02/15.
 */
object PyramidTour extends App {


  //trait Field {
  //  def isValid(p: Piece, o: Orientation): Boolean
  //  val i: Int
  //  val j: Int
  //}
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
  case class Three() extends Piece
  case class Two() extends Piece
  case class One() extends Piece

  trait Orientation
  case class Up()    extends Orientation
  case class Right() extends Orientation
  case class Down()  extends Orientation
  case class Left()  extends Orientation

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

  println(board.foldLeft(BigInt(1))((left, item) => left * BigInt(item._2.size)))
  println(board(Field(0, 0)))


  def next(f: Field, p: Piece, o: Orientation): Field = (p, o) match {
    case (Three(), Up())    => Field(f.i - 3, f.j    )
    case (Three(), Right()) => Field(f.i    , f.j + 3)
    case (Three(), Left())  => Field(f.i    , f.j - 3)
    case (Three(), Down())  => Field(f.i + 3, f.j    )
    case (Two()  , Up())    => Field(f.i - 2, f.j    )
    case (Two()  , Right()) => Field(f.i    , f.j + 2)
    case (Two()  , Left())  => Field(f.i    , f.j - 2)
    case (Two()  , Down())  => Field(f.i + 2, f.j    )
    case (One()  , Up())    => Field(f.i - 2, f.j    )
    case (One()  , Right()) => Field(f.i    , f.j + 1)
    case (One()  , Left())  => Field(f.i    , f.j - 1)
    case (One()  , Down())  => Field(f.i + 1, f.j    )
  }

  // look at hint for stack
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
   */
  def pyramidTour(emptyFields: Set[Field],
            placedFields: List[(Field, Piece, Orientation)],
            availableOptions: List[Set[(Piece, Orientation)]],
            availablePieces: Map[Piece, Int]): List[(Field, Piece, Orientation)] = {

    def availableOptionsAt(f: Field, without: (Piece, Orientation)): Set[(Piece, Orientation)] = board(f).intersect(availableOptions.head - without).toSet

    placedFields match {
      case List( (Field(i, j), p, o, _) ) if (emptyFields.isEmpty && next(Field(i, j), _, _) == Field(0,0)) =>
        // case 1)
        placedFields
      case (Field(i, j), p, o, available) :: xs =>
        val nextField = next(Field(i, j), p, o)
        if (!emptyFields.contains(nextField)) {
          val currentAvailableOptions = availableOptions.head
          if (!currentAvailableOptions.isEmpty) {
            // 2.1)

          } else {
            // 2.2)
          }

        } else {
          val optionsForNextField = board(Field(i, j))
          if (optionsForNextField.exists(availableOptions.head)) {
            // case 3)
          } else {
            // case 4)
            val currentAvailableOptions = availableOptions.head
            if (!currentAvailableOptions.isEmpty) {
              // 4.1)
              val firstOption = currentAvailableOptions.head
              pyramidTour (emptyFields - nextField, // next field is not empty anymore
                          (nextField, firstOption._1, firstOption._2) :: placedFields, // the first option is now placed
                           currentAvailableOptions - firstOption :: availableOptions.tail,
                          availablePieces - )
            } else {
              // 4.2)
            }
          }
        }

      case _ =>
    }

    null
  }

}
