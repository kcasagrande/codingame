package codingame.training.easy.onedspreadsheet

import codingame.InOutTesting
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class Test
  extends AnyFreeSpecLike
  with InOutTesting
{
  "1D Spreadsheet" - {
    implicit val app: App = Solution

    "Simple dependency" in outputWithInput(
      """2
        |VALUE 3 _
        |ADD $0 4"""
        .stripMargin
    ) { _ shouldEqual
        """3
          |7
          |""".stripMargin
    }

    "Coefficients" in outputWithInput(
      """7
        |VALUE 10 _
        |VALUE 3 _
        |MULT $0 $1
        |VALUE 2 _
        |VALUE 4 _
        |MULT $3 $4
        |ADD $2 $5"""
        .stripMargin
    ) {
      _ shouldEqual
        """10
          |3
          |30
          |2
          |4
          |8
          |38
          |""".stripMargin
    }
  }
}
