package codingame.training.easy.movesinmaze

import scala.io.StdIn._

object Solution extends App {
  val Array(width, height) = readLine.split(" ").filterNot(_.isEmpty).map(_.toInt)
  print(
    """###
      |#0#
      |###
      |""".stripMargin
  )
}