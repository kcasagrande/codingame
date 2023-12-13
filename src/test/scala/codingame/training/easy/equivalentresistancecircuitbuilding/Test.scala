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
    "should determine the resistance of a serie of two resistors" in outputWithInput {
      """2
        |A 1
        |B 2
        |( A B )
        |""".stripMargin
    } {
      _ shouldEqual
        """3.0
          |""".stripMargin
    }
    "should determine the resistance of two resistors in parallel" in outputWithInput {
      """2
        |A 4
        |B 4
        |[ A B ]
        |""".stripMargin
    } {
      _ shouldEqual
        """2.0
          |""".stripMargin
    }
    "should determine the resistance of the combined example" in outputWithInput {
      """3
        |A 24
        |B 8
        |C 48
        |[ ( A B ) [ C A ] ]
        |""".stripMargin
    } {
      _ shouldEqual
        """10.7
          |""".stripMargin
    }
  }
}