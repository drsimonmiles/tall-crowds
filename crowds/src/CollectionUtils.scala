import indigo.shared.dice.Dice
import scala.annotation.tailrec

object CollectionUtils {
  def choose[A] (items: Seq[A], dice: Dice): A =
    items (dice.roll (items.size) - 1)

  def chooseWeighted[A] (dice: Dice, weightings: Iterable[(A, Double)]): A = {
    @tailrec
    def chooseFrom (probability: Double, remaining: List[(A, Double)]): A =
      if (probability < remaining.head._2) remaining.head._1
      else chooseFrom (probability - remaining.head._2, remaining.tail)
    chooseFrom (dice.rollDouble, weightings.toList)
  }


  def indicesWhere[A] (sequence: List[A], condition: A => Boolean): List[Int] =
    sequence.zipWithIndex.flatMap {
      case (element, index) => if (condition (element)) Some (index) else None
    }

  def coordinatesWhere[A] (sequences: List[List[A]], condition: A => Boolean): List[(Int, Int)] =
    sequences.zipWithIndex.flatMap {
      case (sequence, index1) =>
        indicesWhere (sequence, condition).map (index2 => (index1, index2))
    }
}
