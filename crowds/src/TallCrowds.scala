import scala.scalajs.js.annotation.JSExportTopLevel
import Assets.{planSpecs, progressBar, progressBase}
import Scenario.{loadScenario, pathComputations}
import Settings.{defaultArrivalChance, progressBarPosition, progressBasePosition}
import indigo._
import indigo.scenes.{Scene, SceneName}

@JSExportTopLevel("IndigoGame")
object TallCrowds extends IndigoGame[GameViewport, ReferenceData, Model, ViewModel] {
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
    NonEmptyList (PrepareScene, SimulationScene)

  def initialScene (bootData: GameViewport): Option[SceneName] =
    Some (PrepareScene.name)

  def setup (bootData: GameViewport,
             assetCollection: AssetCollection,
             dice: Dice): Startup[ReferenceData] =
    assetCollection.findTextDataByName (planSpecs)
      .map (specs => Startup.Success (ReferenceData (specs)))
      .getOrElse (Startup.Failure ("Could not load scenario specs"))

  def initialModel (startupData: ReferenceData): Model = {
    val scenario = loadScenario (startupData.scenarioSpecs)
    Model (
      scenario = scenario,
      pathComputations = pathComputations (scenario),
      bestPath = Map.empty,
      walkers = List.empty,
      stepArrivalChancePerRoute = defaultArrivalChance,
      lastStep = Seconds.zero)
  }

  def initialViewModel(startupData: ReferenceData, model: Model): ViewModel =
    ViewModel (ProgressBar (progressBasePosition, progressBarPosition, progressBase, progressBar))
}
