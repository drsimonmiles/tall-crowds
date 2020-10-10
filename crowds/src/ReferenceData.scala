import Assets.wallGraphic
import Settings.{cellMiddle, cellSize}
import indigo._

case class ReferenceData (scenario: Scenario) {
  val wallGraphics: Group = Group (
    (0 until scenario.plan.width).toList.flatMap (x =>
      (0 until scenario.plan.height).toList.flatMap (y =>
        scenario.plan.grid (x)(y).map (wallGraphic).map (_.moveTo (x * cellSize, y * cellSize))
      )))
}
