import PathSearch.aStarSearch
import Plan.loadPlan
import Route.{PathLookup, loadRoutes}

case class Scenario (plan: Plan, routes: List[Route])

object Scenario {
  def loadScenario (code: String): Scenario =
    loadScenario (code.split ("\n").filter (_.nonEmpty).toList.map (_.toList).transpose)

  def loadScenario (rows: List[List[Char]]): Scenario =
    Scenario (loadPlan (rows), loadRoutes (rows))

  def pathComputations (scenario: Scenario): InterleavedComputation[Scenario, PathLookup] =
    InterleavedComputation[Scenario, PathLookup] (
      currentResult = Map.empty,
      referenceData = scenario,
      steps =
        scenario.routes.flatMap (route =>
          scenario.plan.cells.map (cell => new ComputePath (route, cell, scenario.plan)))
    )
}

class ComputePath (route: Route, start: GridPosition, plan: Plan) extends MonitoredStep[Scenario, PathLookup] {
  val size: Int = 1

  def perform (scenario: Scenario, lookup: PathLookup): PathLookup =
    lookup +
      ((route.id, start) -> aStarSearch (start, route.ends, position => plan.hasWall (position)))
}
