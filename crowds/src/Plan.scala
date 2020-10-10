import CollectionUtils._
import Direction._
import PathSearch.aStarSearch
import Position._
import Wall.wallAtRelativePoint
import indigo.Point

case class Plan (width: Int, height: Int, grid: List[List[Option[WallAngle]]]) {
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

case class Scenario (plan: Plan, routes: List[Route])

object Plan {
  val routeMarkers: List[(Char, Char)] = List (('a', 'A'), ('b', 'B'), ('c', 'C'), ('d', 'D'))

  def loadScenario (code: String): Scenario = {
    val rows = code.split ("\n").filter (_.nonEmpty).toList.map (_.toList).transpose
    val width = rows.head.size
    val height = rows.size
    val walls = rows.map (_.map (_ == 'X'))
    val cells = for (x <- 0 until width; y <- 0 until height) yield GridPosition (x, y)
    val routes = routeMarkers.flatMap {
      case (start, end) =>
        val startPlaces = coordinatesWhere[Char] (rows, _ == start).map (c => GridPosition (c._1, c._2))
        val endPlaces = coordinatesWhere[Char] (rows, _ == end).map (c => GridPosition (c._1, c._2))
        val paths = cells.map (start => start -> aStarSearch (start, endPlaces,
          position => hasWall (position.x, position.y, walls))).toMap
        if (startPlaces.nonEmpty && endPlaces.nonEmpty && startPlaces.forall (start => paths (start).isDefined))
          Some (Route (startPlaces, endPlaces, paths))
        else
          None
    }
    Scenario (Plan (width, height, wallAngles (width, height, walls)), routes)
  }


  private def wallAngles (width: Int, height: Int, walls: List[List[Boolean]]): List[List[Option[WallAngle]]] =
    (0 until width).toList.map (x =>
      (0 until height).toList.map (y =>
        wallAt (x, y, walls)))

  def hasWall (position: GridPosition, plan: Plan): Boolean =
    if (position.x < 0 || position.x >= plan.width || position.y < 0 || position.y >= plan.height) true
    else plan.grid (position.x)(position.y).isDefined

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
