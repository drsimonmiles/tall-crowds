import CollectionUtils.coordinatesWhere
import Route.RouteMarker

case class Route (id: RouteMarker, starts: List[GridPosition], ends: List[GridPosition])

object Route {
  type RouteMarker = Char
  type PathLookup = Map[(RouteMarker, GridPosition), Option[List[GridPosition]]]

  val routeMarkers: List[(RouteMarker, RouteMarker)] = List (('a', 'A'), ('b', 'B'), ('c', 'C'), ('d', 'D'))

  def loadRoutes (rows: List[List[Char]]): List[Route] =
    routeMarkers.flatMap {
      case (start, end) =>
        val startPlaces = coordinatesWhere[Char] (rows, _ == start).map (c => GridPosition (c._1, c._2))
        val endPlaces = coordinatesWhere[Char] (rows, _ == end).map (c => GridPosition (c._1, c._2))
        //val paths = plan.cells.map (start => start -> aStarSearch (start, endPlaces, position => plan.hasWall (position))).toMap
        if (startPlaces.nonEmpty && endPlaces.nonEmpty) Some (Route (start, startPlaces, endPlaces))
        else None
    }

  def nextStep (from: GridPosition, route: Route, bestPath: PathLookup): Option[GridPosition] =
    bestPath.get (route.id, from).flatMap (_.flatMap (_.headOption))
}
