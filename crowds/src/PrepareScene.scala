import Settings._
import indigo._
import indigo.scenes._

object PrepareScene extends Scene[ReferenceData, Model, ViewModel] {
  type SceneModel = Model
  type SceneViewModel = ViewModel

  val name: SceneName = SceneName ("Prepare scene")
  val modelLens: Lens[Model, SceneModel] = Lens.keepLatest
  val viewModelLens: Lens[ViewModel, SceneViewModel] = Lens.keepLatest
  val eventFilters: EventFilters = EventFilters.Default
  val subSystems: Set[SubSystem] = Set ()

  def updateModel (context: FrameContext[ReferenceData], model: Model): GlobalEvent => Outcome[Model] = {
    case FrameTick if !model.pathComputations.isCompleted =>
      Outcome (model.copy (pathComputations = model.pathComputations.performMore (context.gameTime)))
    case FrameTick if model.pathComputations.isCompleted =>
      Outcome (model.copy (bestPath = model.pathComputations.currentResult))
        .addGlobalEvents (SceneEvent.JumpTo (SimulationScene.name))
    case _ =>
      Outcome (model)
  }

  def updateViewModel (context: FrameContext[ReferenceData], model: Model, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome (viewModel.copy (progressBar = viewModel.progressBar.update (model.pathComputations.portionCompleted)))

  def present (context: FrameContext[ReferenceData], model: Model, viewModel: ViewModel): SceneUpdateFragment =
    SceneUpdateFragment.empty
      .addUiLayerNodes (viewModel.progressBar.draw)
      .addUiLayerNodes (Text("P r e p a r i n g :  P l e a s e   w a i t", horizontalCentre, verticalMiddle - 40, 1, Font.fontKey).scaleBy (2, 2).alignCenter)
}
