import indigo.Seconds

object Settings {
  val cellSize = 31
  val cellMiddle = 15

  val defaultArrivalChance = 0.03
  val stepSpeed = Seconds (0.03)

  val bodyRadius = 12
  val tightRadius = 20
  val tightScale = tightRadius * 2.0 / cellSize
  val looseRadius = 40
  val looseScale = looseRadius * 2.0 / cellSize

  val destinationWeight = 1.0F
  val tightWeight = 5.0F
  val looseWeight = 2.0F
  val wallWeight = 0.5F
  val wiggleWeight = 0.8F

  val walkerMemorySize = 10
  val impatienceIncrement = 0.25F

  val viewportWidth = cellSize * 16
  val viewportHeight = cellSize * 16
  val magnificationLevel = 1

  val horizontalCentre = viewportWidth / 2
  val verticalMiddle = viewportHeight / 2
}
