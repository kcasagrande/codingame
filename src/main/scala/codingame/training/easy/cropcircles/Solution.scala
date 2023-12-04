package codingame.training.easy.cropcircles

import scala.io.StdIn._
import scala.util.matching.Regex

case class Point(
  x: Int,
  y: Int
) {
  def <->(other: Point): Double = Math.sqrt(Math.pow(other.x - x, 2) + Math.pow(other.y - y, 2))
}

case class Instruction(center: Point, diameter: Int) {
  def includes(point: Point): Boolean = (point <-> center) <= (diameter / 2)
}
object Instruction {
  val fromInput: Regex = "^([a-s])([a-y])(0|[1-9][0-9]*)$".r
  def fromMatch(x: String, y: String, d: String): Instruction =
    Instruction(Point(x.head - 'a', y.head - 'a'), d.toInt)
}

object Solution extends App {
  val instruction = readLine match {
    case Instruction.fromInput(x, y, d) => Instruction.fromMatch(x, y, d)
  }
  val field = (for {
    y <- 0 until 25
  } yield {
    (for {
      x <- 0 until 19
    } yield {
      if(instruction.includes(Point(x, y))) {
        "  "
      } else {
        "{}"
      }
    }).mkString
  }).mkString("\n")
  println(field)
}