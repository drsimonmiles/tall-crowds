import Direction.directionBetween
import Model._
import Position._
import Settings._
import Walker.walkerAvoidanceDirections
import indigo._
import utest._

object Tests extends TestSuite {
  val tests: Tests =
    utest.Tests {

      "points" - {
        "circle around point" - {
          val centre = Point (10, 10)
          val radius = 5
          val points = Position.onRadiusAround (centre, radius)
        }
      }

      "distancing" - {
        val start = GridPosition (0, 3)
        val end = GridPosition (0, 0)
        val best = (0 to 3).map (y => GridPosition (0, y) -> Some ((y to 3).map (y2 => GridPosition (0, y2)).toList)).toMap
        val route = Route (List (start), List (end), best)
        val me = Walker (gridPositionToMidPoint (start), route, Normal, 156, Nil)
        val you = Walker (moveBy (me.position, 0, 1 - tightRadius), route, Normal, 156, Nil)
        val walkers = List (me, you)

        "away from others" - {
          assert (walkerAvoidanceDirections (me, walkers) == Set (East, SouthEast, South, SouthWest, West))
        }

        "allowed directions" - {
          val plan = Plan (1, 4, List (List.fill[Option[WallAngle]] (4)(None)))
          val waypoint = GridPosition (0, 1)
          val allowed = allowedDirections (me, waypoint, walkers, plan)
          println ("Forward direction: " + directionBetween (me.position, gridPositionToMidPoint (waypoint)))
          println ("Non-backwards: " + directionBetween (me.position, gridPositionToMidPoint (waypoint)).map (_.notAway))
          println ("Walker avoidance directions: " + walkerAvoidanceDirections (me, walkers))
          println (allowed)
          assert (!allowed.contains (North))
        }
      }
    }
}
