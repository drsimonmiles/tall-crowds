import Assets._
import Model._
import Settings._
import indigo._
import indigo.scenes._

object SimulationScene extends Scene[ReferenceData, Model, ViewModel] {
  type SceneModel = Model
  type SceneViewModel = ViewModel

  val name: SceneName = SceneName ("Simulation scene")
  val modelLens: Lens[Model, SceneModel] = Lens.keepLatest
  val viewModelLens: Lens[ViewModel, SceneViewModel] = Lens.keepLatest
  val eventFilters: EventFilters = EventFilters.Default
  val subSystems: Set[SubSystem] = Set ()

  def updateModel (context: FrameContext[ReferenceData], model: Model): GlobalEvent => Outcome[Model] = {
    case FrameTick =>
      if (context.gameTime.running > model.lastStep + stepSpeed)
        model.scenario match {
          case Some (loadedScenario) =>
            Outcome (addWalkers (moveWalkers2 (model, loadedScenario.plan, context.dice),
              loadedScenario, context.dice).copy (lastStep = context.gameTime.running))
          case None =>
            Outcome (model)
        }
      else Outcome (model)
    case _ => Outcome (model)
  }

  def updateViewModel (context: FrameContext[ReferenceData], model: Model, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome (viewModel)

  def present (context: FrameContext[ReferenceData], model: Model, viewModel: ViewModel): SceneUpdateFragment = {
    model.scenario match {
      case Some (loadedScenario) =>
        SceneUpdateFragment.empty
          .addGameLayerNodes (loadedScenario.plan.wallGraphics)
          .addGameLayerNodes (walkerGraphics (model))
      case None =>
        SceneUpdateFragment.empty
    }
  }

  def walkerGraphics (model: Model): Group =
    Group (model.walkers.flatMap (walker =>
      List (walkerImageChoice (walker.position).moveTo (walker.position),
        tightCircle.moveTo (walker.position),
        looseCircle.moveTo (walker.position)
      )))

  def walkerImageChoice (position: Point): Graphic =
    if ((position.x + position.y) % 2 == 0) walker1 else walker2
}
