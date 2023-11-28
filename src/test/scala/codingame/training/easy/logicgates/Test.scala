package codingame.training.easy.logicgates

import codingame.InOutTesting
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class Test
  extends AnyFreeSpecLike
    with InOutTesting {
  implicit val app: App = Solution

  "Logic Gates" - {
    "can handle OR" - {
      "with two signals" - {
        "with a single tick" in outputWithInput {
          """2
            |1
            |IN1 -
            |IN2 _
            |OUT OR IN1 IN2
            |""".stripMargin
        } {
          _ shouldEqual
            """OUT -
              |""".stripMargin
        }
        "with multiple ticks" in outputWithInput {
          """2
            |1
            |IN1 --__
            |IN2 _-_-
            |OUT OR IN1 IN2
            |""".stripMargin
        } {
          _ shouldEqual
            """OUT --_-
              |""".stripMargin
        }
      }
    }
    "can handle AND" - {
      "with two signals" - {
        "with a single tick" in outputWithInput {
          """2
            |1
            |IN1 -
            |IN2 _
            |OUT AND IN1 IN2
            |""".stripMargin
        } {
          _ shouldEqual
            """OUT _
              |""".stripMargin
        }
        "with multiple ticks" in outputWithInput {
          """2
            |1
            |IN1 --__
            |IN2 _-_-
            |OUT AND IN1 IN2
            |""".stripMargin
        } {
          _ shouldEqual
            """OUT _-__
              |""".stripMargin
        }
      }
    }
    "can handle XOR" in outputWithInput {
      """2
        |1
        |IN1 __--
        |IN2 _-_-
        |OUT XOR IN1 IN2
        |""".stripMargin
    } {
      _ shouldEqual
        """OUT _--_
          |""".stripMargin
    }
    "can handle NOR" in outputWithInput {
      """2
        |1
        |IN1 __--
        |IN2 _-_-
        |OUT NOR IN1 IN2
        |""".stripMargin
    } {
      _ shouldEqual
        """OUT -___
          |""".stripMargin
    }
    "can handle NAND" in outputWithInput {
      """2
        |1
        |IN1 __--
        |IN2 _-_-
        |OUT NAND IN1 IN2
        |""".stripMargin
    } {
      _ shouldEqual
        """OUT ---_
          |""".stripMargin
    }
    "can handle NXOR" in outputWithInput {
      """2
        |1
        |IN1 __--
        |IN2 _-_-
        |OUT NXOR IN1 IN2
        |""".stripMargin
    } {
      _ shouldEqual
        """OUT -__-
          |""".stripMargin
    }
    "can handle multiple outputs" in outputWithInput {
      """4
        |3
        |IN1 ____
        |IN2 ----
        |IN3 __--
        |IN4 --__
        |OUT1 AND IN1 IN2
        |OUT2 OR IN3 IN4
        |OUT3 NXOR IN1 IN3
        |""".stripMargin
    } {
      _ shouldEqual
        """OUT1 ____
          |OUT2 ----
          |OUT3 --__
          |""".stripMargin
    }
  }
}
