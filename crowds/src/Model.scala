import CollectionUtils._
import Direction._
import Force._
import Position._
import Route._
import Settings._
import Walker._
import indigo._

case class Model (walkers: List[Walker], stepArrivalChancePerRoute: Double, lastStep: Seconds)

object Model {
  def addWalkers (model: Model, scenario: Scenario, dice: Dice): Model =
    //if (model.walkers.size >= 1) model else
    model.copy (
      walkers = model.walkers ++
        scenario.routes.flatMap (route =>
          if (dice.rollDouble < model.stepArrivalChancePerRoute)
            Some (createWalker (route, dice)).filterNot ( newWalker =>
              model.walkers.exists (existing => inTightSpace (newWalker, existing)))
          else
            None
        ))

  def moveWalkers (model: Model, plan: Plan, dice: Dice): Model =
    model.copy (walkers =
      model.walkers
        .filterNot (walker => walker.route.ends.contains (pointToGridPosition (walker.position)))
        .map { walker =>
          nextStep (pointToGridPosition (walker.position), walker.route) match {
            case None => walker
            case Some (waypoint) =>
              val forwardForce =
                towardsForce (walker.position, gridPositionToMidPoint (waypoint), destinationWeight/* + walker.impatience*/)
              val forces: Seq[Force] =
                forwardForce ::
                  wiggleForce (dice) ::
                  interWalkerForces (walker, model.walkers.filterNot (_ == walker)) ++
                    wallAvoidanceForces (walker, plan)
              val direction = directionGivenForce (combineForces (forces))
              val ensuredDirection =
                if (direction._1 == 0 && direction._2 == 0) directionGivenForce (forwardForce)
                else direction
              walker.moveTo (moveBy (walker.position, ensuredDirection._1, ensuredDirection._2))
          }})

  def moveWalkers2 (model: Model, plan: Plan, dice: Dice): Model =
    model.copy (walkers =
      model.walkers
        .filterNot (walker => walker.route.ends.contains (pointToGridPosition (walker.position)))
        .map (moveWalker (_, model.walkers, plan, dice)))

  def moveWalker (walker: Walker, others: List[Walker], plan: Plan, dice: Dice): Walker =
    nextStep (pointToGridPosition (walker.position), walker.route) match {
      case None => walker
      case Some (waypoint) =>
        val possibilities = allowedDirections (walker, waypoint, others, plan)
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
