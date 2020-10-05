import CollectionUtils.choose
import Force._
import Position._
import Route._
import Settings._
import Walker._
import indigo._

case class Model (walkers: List[Walker], stepArrivalChancePerRoute: Double, lastStep: Seconds)

object Model {
  def addWalkers (model: Model, plan: Plan, dice: Dice): Model =
    //if (model.walkers.size >= 1) model else
    model.copy (
      walkers = model.walkers ++
        plan.routes.flatMap (route =>
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

  def moveWalkers2 (model: Model, plan: Plan, dice: Dice): Model = {
    model.copy (walkers =
      model.walkers
        .filterNot (walker => walker.route.ends.contains (pointToGridPosition (walker.position)))
        .map { walker =>
          nextStep (pointToGridPosition (walker.position), walker.route) match {
            case None => walker
            case Some (waypoint) =>
              val forward = direction (walker.position, gridPositionToMidPoint (waypoint))
              val forwardPosition = moveBy (walker.position, forward._1, forward._2)
              val possibilities =
                directionsNotAway (forward)
                  .intersect (walkerAvoidanceDirections (walker, model.walkers))
                  .map (direction => moveBy (walker.position, direction._1, direction._2))
                  .intersect (plan.accessibleAdjacentsWithRadius (walker.position, tightRadius))
              if (possibilities.isEmpty) walker
              else if (possibilities.contains (forwardPosition)) walker.moveTo (forwardPosition)
              else walker.moveTo (choose (possibilities.toList, dice))
          }})
  }

}
