import Direction._
import Settings._
import indigo.Point

import scala.annotation.tailrec

case class GridPosition (x: Int, y: Int) {
  def moveBy (dx: Int, dy: Int): GridPosition =
    GridPosition (x + dx, y + dy)

  def moveBy (direction: Direction): GridPosition =
    moveBy (direction.dx, direction.dy)

  def distanceTo (otherPosition: GridPosition): Double =
    Math.sqrt ((x - otherPosition.x) * (x - otherPosition.x) + (y - otherPosition.y) * (y - otherPosition.y))

  def adjacent: Set[GridPosition] =
    directions.map (direction => moveBy (direction))
}

object Position {
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

  def moveBy (point: Point, direction: Direction): Point =
    Point (point.x + direction.dx, point.y + direction.dy)

  def moveTowards (point: Point, towards: Point): Point =
    directionBetween (point, towards).map (moveBy (point, _)).getOrElse (point)

  def adjacent (point: Point): Set[Point] =
    directions.map (direction => moveBy (point, direction))

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
