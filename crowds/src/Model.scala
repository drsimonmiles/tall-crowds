import CollectionUtils.choose
import Direction.{directionBetween, directions}
import Position.{gridPositionToMidPoint, moveBy, moveTowards, pointToGridPosition}
import Route.{PathLookup, nextStep}
import Settings.tightRadius
import Walker.{createWalker, inLooseSpace, walkerAvoidanceDirections}
import indigo._

case class Model (scenario: Scenario, pathComputations: InterleavedComputation[Scenario, PathLookup],
  bestPath: PathLookup, walkers: List[Walker], stepArrivalChancePerRoute: Double, lastStep: Seconds)

object Model {
  def addWalkers (model: Model, scenario: Scenario, dice: Dice): Model =
    model.copy (
      walkers = model.walkers ++
        scenario.routes.flatMap (route =>
          if (dice.rollDouble < model.stepArrivalChancePerRoute)
            Some (createWalker (route, dice)).filterNot ( newWalker =>
              model.walkers.exists (existing => inLooseSpace (newWalker, existing)))
          else
            None
        ))

  def moveWalkers (model: Model, plan: Plan, dice: Dice): Model =
    model.copy (walkers =
      model.walkers
        .filterNot (walker => walker.route.ends.contains (pointToGridPosition (walker.position)))
        .map (moveWalker (_, model, dice)))

  def moveWalker (walker: Walker, model: Model, dice: Dice): Walker =
    nextStep (pointToGridPosition (walker.position), walker.route, model.bestPath) match {
      case None => walker
      case Some (waypoint) =>
        val possibilities = allowedDirections (walker, waypoint, model.walkers, model.scenario.plan)
          .map (moveBy (walker.position, _))
        val forwardPosition = moveTowards (walker.position, gridPositionToMidPoint (waypoint))
        if (possibilities.isEmpty) walker
        else if (possibilities.contains (forwardPosition)) walker.moveTo (forwardPosition)
        else walker.moveTo (choose (possibilities.toList, dice))
    }

  def allowedDirections (walker: Walker, nextWaypoint: GridPosition, others: List[Walker], plan: Plan): Set[Direction] =
    directionBetween (walker.position, gridPositionToMidPoint (nextWaypoint))
      .map (_.notAway)          // Start with directions not away from next waypoint
      .getOrElse (directions)   // Or all directions if for some reason at the next waypoint
      .intersect (walkerAvoidanceDirections (walker, others) )   // Remove directions towards walkers where in their social space
      .intersect (plan.accessibleDirectionsWithRadius (walker.position, tightRadius))  // Remove directions bumping into walls
}
