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
    "should encode a rather long message" in outputWithInput {
      """ENCODE
        |7
        |BDFHJLCPRTXVZNYEIWGAKMUSQO
        |AJDKSIRUXBLHWTMCQGZNPYFVOE
        |EKMFLGDQVZNTOWYHXUSPAIBRCJ
        |WEATHERREPORTWINDYTODAY
        |""".stripMargin
    } {
      _ shouldEqual
        """ALWAURKQEQQWLRAWZHUYKVN
          |""".stripMargin
    }
  }
  "Decode" - {
    "should decode a message" in outputWithInput {
      """DECODE
        |9
        |BDFHJLCPRTXVZNYEIWGAKMUSQO
        |AJDKSIRUXBLHWTMCQGZNPYFVOE
        |EKMFLGDQVZNTOWYHXUSPAIBRCJ
        |PQSACVVTOISXFXCIAMQEM
        |""".stripMargin
    } {
      _ shouldEqual
        """EVERYONEISWELCOMEHERE
          |""".stripMargin
    }
  }
}