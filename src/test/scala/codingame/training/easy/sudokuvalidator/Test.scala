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

  }
}
