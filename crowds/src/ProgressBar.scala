import indigo._

case class ProgressBar (
  position: Point,
  barPosition: Point,
  base: Graphic,
  bar: Graphic,
  progress: Double = 0.0) {

  def update (updatedProgress: Double): ProgressBar =
    this.copy (progress = updatedProgress)

  def draw: Group =
    Group (
      base.moveTo (position),
      bar.moveTo (barPosition)
        .withCrop (0, 0, (bar.lazyBounds.width * progress).toInt, bar.lazyBounds.height))
}
