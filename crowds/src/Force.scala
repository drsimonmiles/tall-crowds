import indigo.Point

case class Force (dx: Float, dy: Float, strength: Float) {
  def asUnitDirection: Force = {
    val scale = Math.sqrt (dx * dx + dy * dy).toFloat
    val unitdx = dx / scale
    val unitdy = dy / scale
    Force (unitdx, unitdy, strength)
  }
}

object Force {
  def apply (direction: (Int, Int), strength: Float): Force =
    Force (direction._1, direction._2, strength)

  def towardsForce (position: Point, target: Point, strength: Float): Force =
    Force (target.x - position.x, target.y - position.y, strength)

  def awayForce (position: Point, deterrent: Point, strength: Float): Force =
    Force (position.x - deterrent.x, position.y - deterrent.y, strength)

  def combineForces (forces: Seq[Force]): Force = {
    val direction =
      forces.map (_.asUnitDirection)
        .map[(Float, Float)] (f => (f.dx * f.strength, f.dy * f.strength))
        .reduceLeft[(Float, Float)] {
          case (force1, force2) =>
            (force1._1 + force2._1, force1._2 + force2._2)
        }
    Force (direction._1, direction._2, 1)
  }

  def directionGivenForce (force: Force): (Int, Int) = {
    val unit = force.asUnitDirection
    (Math.round (unit.dx), Math.round (unit.dy))
  }
}
