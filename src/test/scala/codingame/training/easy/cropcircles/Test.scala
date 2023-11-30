package codingame.training.easy.cropcircles

import codingame.InOutTesting
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class Test
  extends AnyFreeSpecLike
    with InOutTesting {
  implicit val app: App = Solution
  "Crop circles" - {
    "Basic" in outputWithInput {
      """fg9 ls11 oe7
        |""".stripMargin
    } {
      _ shouldEqual
        """{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}
          |{}{}{}{}{}{}{}{}{}{}{}{}{}      {}{}{}
          |{}{}{}          {}{}{}{}          {}{}
          |{}{}              {}{}              {}
          |{}                  {}              {}
          |{}                  {}              {}
          |{}                  {}{}          {}{}
          |{}                  {}{}{}      {}{}{}
          |{}                  {}{}{}{}{}{}{}{}{}
          |{}{}              {}{}{}{}{}{}{}{}{}{}
          |{}{}{}          {}{}{}{}{}{}{}{}{}{}{}
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