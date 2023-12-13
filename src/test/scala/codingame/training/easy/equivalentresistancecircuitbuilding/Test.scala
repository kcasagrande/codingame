package codingame.training.easy.equivalentresistancecircuitbuilding

import codingame.InOutTesting
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class Test
  extends AnyFreeSpecLike
    with InOutTesting {
  implicit val app: App = Solution
  "A circuit" - {
    "should determine the resistance of one single resistor" in outputWithInput {
      """1
        |A 1
        |A
        |""".stripMargin
    } {
      _ shouldEqual
        """1.0
          |""".stripMargin
    }
  }
}