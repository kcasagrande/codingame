package codingame.training.easy.cropcircles

import scala.io.StdIn._
import scala.util.matching.Regex

case class Instruction(x: Int, y: Int)
object Instruction {
  val fromInput: Regex = "^([a-z])([a-z])(0|[1-9][0-9]*)$".r
  def fromMatch(x: String, y: String): Instruction =
    Instruction(x.head - 'a', y.head - 'a')
}

object Solution extends App {
  val instruction = readLine match {
    case Instruction.fromInput(x, y, _) => Instruction.fromMatch(x, y)
  }
  val field = (for {
    y <- 0 until 25
  } yield {
    (for {
      x <- 0 until 19
    } yield {
      if(x == instruction.x && y == instruction.y) {
        "  "
      } else {
        "{}"
      }
    }).mkString
  }).mkString("\n")
  println(field)
}