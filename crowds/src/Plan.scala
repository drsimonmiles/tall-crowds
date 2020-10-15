import Assets.wallGraphic
import Direction._
import Position._
import Settings._
import Wall.wallAtRelativePoint
import indigo.{Group, Point}

case class Plan (grid: List[List[Option[WallAngle]]]) {
  val width: Int = grid.head.size
  val height: Int = grid.size
  val cells: List[GridPosition] =
    (for (x <- 0 until width; y <- 0 until height) yield GridPosition (x, y)).toList

  def hasWall (position: GridPosition): Boolean =
    !inRange (position) || grid (position.x)(position.y).isDefined

  val wallGraphics: Group = Group (
    (0 until width).toList.flatMap (x =>
      (0 until height).toList.flatMap (y =>
        grid (x)(y).map (wallGraphic).map (_.moveTo (x * cellSize, y * cellSize)))))

  def wallAt (position: GridPosition): Option[WallAngle] =
    if (!inRange (position)) None else grid (position.x)(position.y)

  def wallAtPoint (point: Point): Boolean =
    wallAt (pointToGridPosition (point)).exists (angle =>
      wallAtRelativePoint (pointRelativeToGridPosition (point), angle))

  def inRange (position: GridPosition): Boolean =
    position.x >= 0 && position.x < width && position.y >= 0 && position.y < height

  def pointInRange (point: Point): Boolean =
    inRange (pointToGridPosition (point))

  def accessibleAdjacents (point: Point): Set[Point] =
    adjacent (point).filter (point => pointInRange (point) && !wallAtPoint (point))

  def accessibleAdjacentsWithRadius (point: Point, radius: Int): Set[Point] =
    adjacent (point).filter (moved => pointInRange (moved) && !onRadiusAround (moved, radius).exists (wallAtPoint))

  def accessibleDirectionsWithRadius (point: Point, radius: Int): Set[Direction] =
    directions.filter { direction =>
      val moved = moveBy (point, direction)
      pointInRange (moved) && !onRadiusAround (moved, radius).exists (wallAtPoint)
    }
}

object Plan {
  def loadPlan (rows: List[List[Char]]): Plan =
    Plan (wallAngles (rows.map (_.map (_ == 'X'))))

  private def wallAngles (walls: List[List[Boolean]]): List[List[Option[WallAngle]]] =
    walls.head.indices.toList.map (x =>
      walls.indices.toList.map (y =>
        wallAt (x, y, walls)))

  private def hasWall (x: Int, y: Int, walls: List[List[Boolean]]): Boolean =
    if (x < 0 || x >= walls.size || y < 0 || y >= walls.head.size) true else walls (x)(y)

  private def wallAt (x: Int, y: Int, walls: List[List[Boolean]]): Option[WallAngle] =
    if (!hasWall (x, y, walls)) None
    else
      (hasWall (x - 1, y, walls), hasWall (x, y - 1, walls), hasWall (x + 1, y, walls), hasWall (x, y + 1, walls)) match {
        case (false, true, true, true) => Some (RightWall)
        case (true, false, true, true) => Some (DownWall)
        case (true, true, false, true) => Some (LeftWall)
        case (true, true, true, false) => Some (UpWall)
        case (true, false, false, true) => Some (BottomLeftDiagonal)
        case (true, true, false, false) => Some (TopLeftDiagonal)
        case (false, true, true, false) => Some (TopRightDiagonal)
        case (false, false, true, true) => Some (BottomRightDiagonal)
        case _ => Some (MidWall)
      }
}
