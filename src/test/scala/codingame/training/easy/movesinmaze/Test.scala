package codingame.training.easy.movesinmaze

import codingame.InOutTesting
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class Test
  extends AnyFreeSpecLike
    with InOutTesting {
  implicit val app: App = Solution
  "Moves in Maze" - {
    "should reach the starting position with no moves" in outputWithInput {
      """3 3
        |###
        |#S#
        |###
        |""".stripMargin
    } {
      _ shouldEqual
        """###
          |#0#
          |###
          |""".stripMargin
    }
    "should reach the only free space, adjacent to the starting position, in one move" in outputWithInput {
      """4 3
        |####
        |#S.#
        |####
        |""".stripMargin
    } {
      _ shouldEqual
        """####
          |#01#
          |####
          |""".stripMargin
    }
  }
}