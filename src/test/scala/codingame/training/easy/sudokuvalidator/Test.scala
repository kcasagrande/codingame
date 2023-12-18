package codingame.training.easy.sudokuvalidator

import codingame.InOutTesting
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class Test
  extends AnyFreeSpecLike
    with InOutTesting {
  "Sudoku Validator" - {
    implicit val app: App = Solution

    "should valid a correct grid" in outputWithInput(
      """1 2 3 4 5 6 7 8 9
        |4 5 6 7 8 9 1 2 3
        |7 8 9 1 2 3 4 5 6
        |9 1 2 3 4 5 6 7 8
        |3 4 5 6 7 8 9 1 2
        |6 7 8 9 1 2 3 4 5
        |8 9 1 2 3 4 5 6 7
        |2 3 4 5 6 7 8 9 1
        |5 6 7 8 9 1 2 3 4
        |""".stripMargin
    ) {
      _ shouldEqual
        """true
          |""".stripMargin
    }

    "should detect a row error" in outputWithInput(
      """4 3 2 2 6 9 7 8 1
        |6 8 5 5 7 1 4 9 3
        |1 9 7 8 3 4 5 6 2
        |8 2 6 1 9 5 3 4 7
        |3 7 4 6 8 2 9 1 5
        |9 5 1 7 4 3 6 2 8
        |5 1 9 3 2 6 8 7 4
        |2 4 8 9 5 7 1 3 6
        |7 6 3 4 1 8 2 5 9
        |""".stripMargin
    ) {
      _ shouldEqual
        """false
          |""".stripMargin
    }

    "should detect a non consecutive row error" in outputWithInput(
      """4 3 2 6 2 9 7 8 1
        |6 8 5 7 5 1 4 9 3
        |1 9 7 3 8 4 5 6 2
        |8 2 6 9 1 5 3 4 7
        |3 7 4 8 6 2 9 1 5
        |9 5 1 4 7 3 6 2 8
        |5 1 9 2 3 6 8 7 4
        |2 4 8 5 9 7 1 3 6
        |7 6 3 1 4 8 2 5 9
        |""".stripMargin
    ) {
      _ shouldEqual
        """false
          |""".stripMargin
    }

    "should detect a column error" in outputWithInput(
      """1 2 3 4 5 6 7 8 9
        |1 5 6 7 8 9 2 4 3
        |7 8 9 1 2 3 4 5 6
        |9 1 2 3 4 5 6 7 8
        |3 4 5 6 7 8 9 1 2
        |6 7 8 9 1 2 3 4 5
        |8 9 1 2 3 4 5 6 7
        |2 3 4 5 6 7 8 9 1
        |5 6 7 8 9 1 2 3 4
        |""".stripMargin
      ) {
      _ shouldEqual
        """false
          |""".stripMargin
    }

    "should detect a subgrid error" in outputWithInput(
      """4 3 5 2 6 9 7 8 1
        |6 8 2 5 7 1 4 9 3
        |8 9 7 1 3 4 5 6 2
        |1 2 6 8 9 5 3 4 7
        |3 7 4 6 8 2 9 1 5
        |9 5 1 7 4 3 6 2 8
        |5 1 9 3 2 6 8 7 4
        |2 4 8 9 5 7 1 3 6
        |7 6 3 4 1 8 2 5 9
        |""".stripMargin
    ) {
      _ shouldEqual
        """false
          |""".stripMargin
    }

  }
}
