sealed trait Personality
case object Impatient extends Personality
case object Normal extends Personality
case object Cautious extends Personality

object Personality {
  val personalities = List (Impatient, Normal, Cautious)
}
