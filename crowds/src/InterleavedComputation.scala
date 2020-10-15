import scala.annotation.tailrec
import indigo.GameTime

trait MonitoredStep[ReferenceData, ResultModel] {
  val size: Int
  def perform (reference: ReferenceData, current: ResultModel): ResultModel
}

case class InterleavedComputation[ReferenceData, ResultModel] (
  currentResult: ResultModel,
  referenceData: ReferenceData,
  steps: List[MonitoredStep[ReferenceData, ResultModel]],
  averageTimePerUnit: Double = 0.0,
  sizeCompleted: Int = 0
) {
  lazy val isCompleted: Boolean = steps.isEmpty
  lazy val sizeRemaining: Int = steps.map (_.size).sum
  lazy val portionCompleted: Double = sizeCompleted.toDouble / (sizeCompleted + sizeRemaining)

  def performMore (gameTime: GameTime): InterleavedComputation[ReferenceData, ResultModel] =
    performMoreFrom (gameTime.frameDuration.value, 0L)

  /**
   * Performs at least one step, and then more if there is still time. */
  @tailrec
  private def performMoreFrom (frameDuration: Long, taken: Long): InterleavedComputation[ReferenceData, ResultModel] =
    steps match {
      case Nil => this
      case first :: _ =>
        val start = System.currentTimeMillis
        val updated = performStep
        if ((taken + (first.size * averageTimePerUnit).toLong) > frameDuration) updated
        else updated.performMoreFrom (frameDuration, taken + System.currentTimeMillis - start)
    }

  def performStep: InterleavedComputation[ReferenceData, ResultModel] =
    steps match {
      case Nil => this
      case first :: rest =>
        val start = System.currentTimeMillis
        val result = first.perform (referenceData, currentResult)
        val duration = System.currentTimeMillis - start
        val nowCompleted = sizeCompleted + first.size
        InterleavedComputation (result, referenceData, rest,
          (averageTimePerUnit * sizeCompleted + duration) / nowCompleted, nowCompleted)
    }
}
