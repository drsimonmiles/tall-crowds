import Settings._
import indigo.Point

import scala.annotation.tailrec

case class GridPosition (x: Int, y: Int) {
  def moveBy (dx: Int, dy: Int): GridPosition =
    GridPosition (x + dx, y + dy)

  def distanceTo (otherPosition: GridPosition): Double =
    Math.sqrt ((x - otherPosition.x) * (x - otherPosition.x) + (y - otherPosition.y) * (y - otherPosition.y))

  def adjacent: Set[GridPosition] =
    Position.directions.map (coords => moveBy (coords._1, coords._2))
}

object Position {
  val directions = Set ((0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1))

  def gridPositionToTopLeftPoint (gridPosition: GridPosition): Point =
    Point (gridPosition.x * cellSize, gridPosition.y * cellSize)

  def gridPositionToMidPoint (gridPosition: GridPosition): Point =
    Point (gridPosition.x * cellSize + cellMiddle, gridPosition.y * cellSize + cellMiddle)

  def pointToGridPosition (point: Point): GridPosition =
    GridPosition (point.x / cellSize, point.y / cellSize)

  def pointRelativeToGridPosition (absolutePoint: Point): Point = {
    val gridPosition = pointToGridPosition (absolutePoint)
    Point (absolutePoint.x - gridPosition.x * cellSize, absolutePoint.y - gridPosition.y * cellSize)
  }

  def moveBy (point: Point, dx: Int, dy: Int): Point =
    Point (point.x + dx, point.y + dy)

  def adjacent (point: Point): Set[Point] =
    directions.map (coords => moveBy (point, coords._1, coords._2))

  def direction (from: Point, to: Point): (Int, Int) = {
    val dx = to.x - from.x
    val dy = to.y - from.y
    val scale = Math.sqrt (dx * dx + dy * dy).toFloat
    val unitdx = dx / scale
    val unitdy = dy / scale
    (Math.round (unitdx), Math.round (unitdy))
  }

  def directionsNotAway (direction: (Int, Int)): Set[(Int, Int)] = {
    val oppositeX = if (direction._1 == 0) None else Some (-direction._1)
    val oppositeY = if (direction._2 == 0) None else Some (-direction._2)
    directions.filter (d =>
      (!oppositeX.contains (d._1) || d._2 == direction._2) &&
        (!oppositeY.contains (d._2) || d._1 == direction._1))
  }

  def directionsNotTowards (direction: (Int, Int)): Set[(Int, Int)] =
    directionsNotAway ((-direction._1, -direction._2))

  /** Determines the pixel points in a circle of a given radius around a given point
   * using Bresenham’s circle drawing algorithm */
  def onRadiusAround (point: Point, radius: Int): Set[Point] = {
    def circlePoints (x: Int, y: Int): Set[Point] =
      Set (moveBy (point, x, y), moveBy (point, -x, y), moveBy (point, -x, -y), moveBy (point, x, -y),
        moveBy (point, y, x), moveBy (point, -y, x), moveBy (point, -y, -x), moveBy (point, y, -x))
    @tailrec
    def plotFrom (x: Int, y: Int, d: Int, already: Set[Point]): Set[Point] =
      if (y < x) already
      else if (d > 0)
        plotFrom (x + 1, y - 1, d + 4 * (x - y) + 10, already ++ circlePoints (x, y))
      else
        plotFrom (x + 1, y, d + 4 * x + 6, already ++ circlePoints (x, y))
    plotFrom (0, radius, 3 - 2 * radius, Set.empty)
  }
}
