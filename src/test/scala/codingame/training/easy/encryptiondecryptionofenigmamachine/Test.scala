package codingame.training.easy.encryptiondecryptionofenigmamachine

import codingame.InOutTesting
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class Test
  extends AnyFreeSpecLike
    with InOutTesting {
  implicit val app: App = Solution
  "Encode" - {
    "should encode a simple three letters message" in outputWithInput {
      """ENCODE
        |4
        |BDFHJLCPRTXVZNYEIWGAKMUSQO
        |AJDKSIRUXBLHWTMCQGZNPYFVOE
        |EKMFLGDQVZNTOWYHXUSPAIBRCJ
        |AAA
        |""".stripMargin
    } {
      _ shouldEqual
        """KQF
          |""".stripMargin
    }
  }
}