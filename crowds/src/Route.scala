case class Route (starts: List[GridPosition], ends: List[GridPosition],
                  bestPath: Map[GridPosition, Option[List[GridPosition]]])

object Route {
  def nextStep (from: GridPosition, route: Route): Option[GridPosition] =
    route.bestPath.get (from).flatMap (_.flatMap (_.headOption))
}
