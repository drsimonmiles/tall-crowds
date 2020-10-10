import scala.annotation.tailrec

object PathSearch {
  /** The solution returned does not include the start state itself. */
  def aStarSearch (start: GridPosition, goals: List[GridPosition], blocked: GridPosition => Boolean): Option[List[GridPosition]] = {
    // Working solution is path so far in reverse (head is point reached, last is start), reverse on returning
    type SolveState = List[GridPosition]

    def cost (state: SolveState): Double =
      state.size + start.distanceTo (state.head)

    def compare (stateA: SolveState, stateB: SolveState): Boolean =
      cost (stateA) < cost (stateB)

    // Returns true if the LHS reaches the same state as the RHS but in equal or less actions
    def subsumes (existing: SolveState, newState: SolveState): Boolean =
      existing.head == newState.head && existing.size <= newState.size

    // Perform the a-star search recursively
    @tailrec
    def solve (states: List[SolveState], tried: List[SolveState]): Option[List[GridPosition]] =
      if (states.isEmpty) None
      else {
        val state = states.head
        if (goals.contains (state.head)) Some (state.reverse.tail)
        else {
          val filtered = state.head.adjacent.filterNot (blocked).map (_ :: state)
            .filter (state => !tried.exists (subsumes (_, state)))
          solve ((states.tail ++ filtered).sortWith (compare), tried ++ filtered)
        }
    }

    solve (List (List (start)), List (List (start)))
  }
}
