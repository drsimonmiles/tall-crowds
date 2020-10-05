import Assets.planSpecs
import Plan.loadPlan
import Settings.defaultArrivalChance
import indigo._
import indigo.scenes.{Scene, SceneName}

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object TallCrowds extends IndigoGame[GameViewport, ReferenceData, Model, ViewModel] {
  def initialModel (startupData: ReferenceData): Model =
    Model (List.empty, defaultArrivalChance, Seconds (0))

  def boot(flags: Map[String, String]): BootResult[GameViewport] = {
    val assetPath: String = flags.getOrElse ("baseUrl", "")
    val config = GameConfig (
      viewport = GameViewport (Settings.viewportWidth, Settings.viewportHeight),
      frameRate = 60,
      clearColor = ClearColor.White,
      magnification = Settings.magnificationLevel)

    BootResult (config, config.viewport)
      .withAssets (Assets.assets (assetPath))
      .withFonts (Font.fontInfo)
  }

  /** Three scenes: start screen, levels choice, game play screen */
  def scenes (bootData: GameViewport): NonEmptyList[Scene[ReferenceData, Model, ViewModel]] =
    NonEmptyList (SimulationScene)

  def initialScene (bootData: GameViewport): Option[SceneName] =
    Some (SimulationScene.name)

  def setup (bootData: GameViewport,
             assetCollection: AssetCollection,
             dice: Dice): Startup[ReferenceData] =
    assetCollection.findTextDataByName (planSpecs).map (loadPlan)
      .map (plan => Startup.Success (ReferenceData (plan)))
      .getOrElse (Startup.Failure ("Could not load plan"))

  def initialViewModel(startupData: ReferenceData, model: Model): ViewModel =
    ViewModel ()
}
