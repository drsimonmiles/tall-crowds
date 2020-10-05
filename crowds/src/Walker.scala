import CollectionUtils.choose
import Force._
import HeightDistribution.chooseHeight
import Personality.personalities
import Position._
import Settings._
import Wall._
import indigo.Point
import indigo.shared.dice.Dice

case class Walker (position: Point, route: Route, personality: Personality, height: Int, memory: List[Point]) {
  def moveTo (newPosition: Point): Walker =
    copy (position = newPosition, memory = memory.take (walkerMemorySize) :+ newPosition)

  def impatience: Float =
    memory.headOption.map (recent => memory.size - recent.distanceTo (position).toFloat).getOrElse (0F) * impatienceIncrement
}

object Walker {
  def createWalker (route: Route, dice: Dice): Walker =
    Walker (gridPositionToMidPoint (choose (route.starts, dice)), route,
      choose (personalities, dice), chooseHeight (dice), Nil)

  def inTightSpace (walker: Walker, other: Walker): Boolean =
    walker.position.distanceTo (other.position) < tightRadius

  def inLooseSpace (walker: Walker, other: Walker): Boolean =
    walker.position.distanceTo (other.position) < looseRadius

  def walkerAvoidanceDirections (walker: Walker, others: List[Walker]): Set[(Int, Int)] =
    others.foldLeft (directions) { case (remaining, other) =>
      if (inLooseSpace (walker, other))
        remaining.intersect (directionsNotTowards (direction (walker.position, other.position)))
      else remaining
    }

  def interWalkerForces (walker: Walker, others: List[Walker]): List[Force] =
    others.flatMap { other =>
      if (inTightSpace (walker, other)) Some (awayForce (walker.position, other.position, tightWeight))
      else if (inLooseSpace (walker, other)) Some (awayForce (walker.position, other.position, looseWeight))
      else None
    }

  def wallAvoidanceForces (walker: Walker, plan: Plan): List[Force] =
    pointToGridPosition (walker.position).adjacent.flatMap (adjacent =>
      plan.wallAt (adjacent).flatMap (angle =>
        if (crossesAdjacentWall (walker.position, tightRadius, adjacent, angle))
          Some (awayForce (walker.position, centrePoint (adjacent, angle), wallWeight))
        else None
      )
    ).toList

  def wiggleForce (dice: Dice): Force =
    Force (choose (directions.toList, dice), wiggleWeight)
}

