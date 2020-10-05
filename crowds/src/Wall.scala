import Position._
import Settings.{cellMiddle, cellSize}
import indigo.Point

sealed trait WallAngle
case object MidWall extends WallAngle
case object LeftWall extends WallAngle
case object RightWall extends WallAngle
case object UpWall extends WallAngle
case object DownWall extends WallAngle
case object BottomLeftDiagonal extends WallAngle
case object TopLeftDiagonal extends WallAngle
case object TopRightDiagonal extends WallAngle
case object BottomRightDiagonal extends WallAngle

object Wall {
  val sinCosAngle: Double = Math.cos (Math.PI / 4.0)

  def crossesAdjacentWall (position: Point, radius: Int, wallGridPosition: GridPosition, angle: WallAngle): Boolean = {
    val wallPoint = gridPositionToTopLeftPoint (wallGridPosition)
    val diagonal = radius * sinCosAngle
    angle match {
      case LeftWall =>
        position.y >= wallPoint.y && position.y < wallPoint.y + cellSize &&
        position.x - radius < wallPoint.x + cellSize
      case RightWall =>
        position.y >= wallPoint.y && position.y < wallPoint.y + cellSize &&
          position.x + radius <= wallPoint.x
      case UpWall =>
        position.x >= wallPoint.x && position.x < wallPoint.x + cellSize &&
          position.y - radius < wallPoint.y + cellSize
      case DownWall =>
        position.x >= wallPoint.x && position.x < wallPoint.x + cellSize &&
          position.y + radius <= wallPoint.y
      case TopLeftDiagonal =>
        position.x < wallPoint.x + cellSize && position.y >= wallPoint.y &&
        position.y - diagonal <= wallPoint.y + cellSize - 1 - (position.x - diagonal - wallPoint.x)
      case TopRightDiagonal =>
        position.x >= wallPoint.x && position.y >= wallPoint.y &&
        position.y - radius * sinCosAngle <= wallPoint.y + (position.x + radius * sinCosAngle - wallPoint.x)
      case BottomRightDiagonal =>
        position.x >= wallPoint.x && position.y < wallPoint.y + cellSize &&
        position.y + radius * sinCosAngle >= wallPoint.y + cellSize - 1 - (position.x + radius * sinCosAngle - wallPoint.x)
      case BottomLeftDiagonal =>
        position.x < wallPoint.x + cellSize && position.y >= wallPoint.y &&
        position.y + radius * sinCosAngle >= wallPoint.y + (position.x - radius * sinCosAngle - wallPoint.x)
      case _ => false
    }
  }

  def centrePoint (wallPosition: GridPosition, angle: WallAngle): Point =
    angle match {
      case TopLeftDiagonal => gridPositionToTopLeftPoint (wallPosition)
      case TopRightDiagonal => moveBy (gridPositionToTopLeftPoint (wallPosition), cellSize - 1, 0)
      case BottomRightDiagonal => moveBy (gridPositionToTopLeftPoint (wallPosition), cellSize - 1, cellSize - 1)
      case BottomLeftDiagonal => moveBy (gridPositionToTopLeftPoint (wallPosition), 0, cellSize - 1)
      case _ => gridPositionToMidPoint (wallPosition)
    }

  def wallAtRelativePoint (relative: Point, angle: WallAngle): Boolean =
    angle match {
      case TopLeftDiagonal => relative.y <= cellSize - 1 - relative.x
      case TopRightDiagonal => relative.y <= relative.x
      case BottomRightDiagonal => relative.y >= cellSize - 1 - relative.x
      case BottomLeftDiagonal => relative.y >= relative.x
      case _ => true
    }
}