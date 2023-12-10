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
    "should reach another only free space, adjacent to the starting position, in one move" in outputWithInput {
      """4 3
        |####
        |#.S#
        |####
        |""".stripMargin
    } {
      _ shouldEqual
        """####
          |#10#
          |####
          |""".stripMargin
    }
    "should reach two free spaces adjacent to the starting position, in one move each" in outputWithInput {
      """5 3
        |#####
        |#.S.#
        |#####
        |""".stripMargin
    } {
      _ shouldEqual
        """#####
          |#101#
          |#####
          |""".stripMargin
    }
    "should reach a free space two moves away from the starting position" in outputWithInput {
      """5 3
        |#####
        |#S..#
        |#####
        |""".stripMargin
    } {
      _ shouldEqual
        """#####
          |#012#
          |#####
          |""".stripMargin
    }
    "should never reach an unreachable space" in outputWithInput {
      """6 3
        |######
        |#S.#.#
        |######
        |""".stripMargin
    } {
      _ shouldEqual
        """######
          |#01#.#
          |######
          |""".stripMargin
    }
    "should handle a loop" in outputWithInput {
      """5 5
        |#####
        |#S..#
        |#.#.#
        |#...#
        |#####
        |""".stripMargin
    } {
      _ shouldEqual
        """#####
          |#012#
          |#1#3#
          |#234#
          |#####
          |""".stripMargin
    }
  }
}