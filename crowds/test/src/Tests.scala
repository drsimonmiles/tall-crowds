import indigo._
import scala.io.Source
import utest._

object Tests extends TestSuite {
  val tests: Tests =
    utest.Tests {

      "points" - {
        "circle around point" - {
          val centre = Point (10, 10)
          val radius = 5
          println (Position.onRadiusAround (centre, radius))
        }
      }
    }
}
