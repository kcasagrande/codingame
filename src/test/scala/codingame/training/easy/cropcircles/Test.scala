package codingame.training.easy.cropcircles

import codingame.InOutTesting
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class Test
  extends AnyFreeSpecLike
    with InOutTesting {
  implicit val app: App = Solution
  "Crop circles" - {
    "should mow the smallest circle" in outputWithInput {
      """oe1
        |""".stripMargin
    } {
      _ shouldEqual
        """{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}  {}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |""".stripMargin
    }
    "should mow a rather small circle" in outputWithInput {
      """oe3
        |""".stripMargin
    } {
      _ shouldEqual
        """{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}      {}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}      {}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}      {}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |""".stripMargin
    }
    "should mow a large circle" in outputWithInput {
      """ls11
        |""".stripMargin
    } {
      _ shouldEqual
        """{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}          {}{}{}{}{}
          |{}{}{}{}{}{}{}{}              {}{}{}{}
          |{}{}{}{}{}{}{}                  {}{}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}{}                  {}{}{}
          |{}{}{}{}{}{}{}{}              {}{}{}{}
          |{}{}{}{}{}{}{}{}{}          {}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |""".stripMargin
    }
    "should mow multiple circles" in outputWithInput {
      """oe7 ls11
        |""".stripMargin
    } {
      _ shouldEqual
        """{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}      {}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}          {}{}
          |{}{}{}{}{}{}{}{}{}{}{}              {}
          |{}{}{}{}{}{}{}{}{}{}{}              {}
          |{}{}{}{}{}{}{}{}{}{}{}              {}
          |{}{}{}{}{}{}{}{}{}{}{}{}          {}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}      {}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}          {}{}{}{}{}
          |{}{}{}{}{}{}{}{}              {}{}{}{}
          |{}{}{}{}{}{}{}                  {}{}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}                      {}{}
          |{}{}{}{}{}{}{}                  {}{}{}
          |{}{}{}{}{}{}{}{}              {}{}{}{}
          |{}{}{}{}{}{}{}{}{}          {}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |""".stripMargin
    }
  }
}