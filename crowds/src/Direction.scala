import indigo._

sealed trait Direction {
  val dx: Int
  val dy: Int
  val opposite: Direction
  val notAway: Set[Direction]
  lazy val notTowards: Set[Direction] = opposite.notAway
  val toTuple: (Int, Int) = (dx, dy)
}
case object North extends Direction {
  val dx = 0
  val dy = -1
  val opposite = South
  val notAway = Set (West, NorthWest, North, NorthEast, East)
}
case object NorthEast extends Direction {
  val dx = 1
  val dy = -1
  val opposite = SouthWest
  val notAway = Set (NorthWest, North, NorthEast, East, SouthEast)
}
case object East extends Direction {
  val dx = 1
  val dy = 0
  val opposite = West
  val notAway = Set (North, NorthEast, East, SouthEast, South)
}
case object SouthEast extends Direction {
  val dx = 1
  val dy = 1
  val opposite = NorthWest
  val notAway = Set (NorthEast, East, SouthEast, South, SouthWest)
}
case object South extends Direction {
  val dx = 0
  val dy = 1
  val opposite = North
  val notAway = Set (East, SouthEast, South, SouthWest, West)
}
case object SouthWest extends Direction {
  val dx = -1
  val dy = 1
  val opposite = NorthEast
  val notAway = Set (SouthEast, South, SouthWest, West, NorthWest)
}
case object West extends Direction {
  val dx = -1
  val dy = 0
  val opposite = East
  val notAway = Set (South, SouthWest, West, NorthWest, North)
}
case object NorthWest extends Direction {
  val dx = -1
  val dy = -1
  val opposite = SouthEast
  val notAway = Set (SouthWest, West, NorthWest, North, NorthEast)
}

object Direction {
  val directions: Set[Direction] =
    Set (North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)

  def apply (dx: Int, dy: Int): Option[Direction] =
    (dx, dy) match {
      case (0, -1) => Some (North)
      case (1, -1) => Some (NorthEast)
      case (1,  0) => Some (East)
      case (1,  1) => Some (SouthEast)
      case (0,  1) => Some (South)
      case (-1, 1) => Some (SouthWest)
      case (-1, 0) => Some (West)
      case (-1, -1) => Some (NorthWest)
      case _ => None
    }

  def directionBetween (from: Point, to: Point): Option[Direction] = {
    val dx = to.x - from.x
    val dy = to.y - from.y
    val scale = Math.sqrt (dx * dx + dy * dy).toFloat
    val unitdx = dx / scale
    val unitdy = dy / scale
    Direction (Math.round (unitdx), Math.round (unitdy))
  }

}
