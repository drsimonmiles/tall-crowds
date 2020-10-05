import Assets.wallGraphic
import Settings.{cellMiddle, cellSize}
import indigo._

case class ReferenceData (plan: Plan) {
  val wallGraphics: Group = Group (
    (0 until plan.width).toList.flatMap (x =>
      (0 until plan.height).toList.flatMap (y =>
        plan.grid (x)(y).map (wallGraphic).map (_.moveTo (x * cellSize, y * cellSize))
      )))
}
