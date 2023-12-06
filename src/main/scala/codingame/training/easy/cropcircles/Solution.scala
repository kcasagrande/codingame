package codingame.training.easy.cropcircles

import scala.io.StdIn._
import scala.util.matching.Regex

case class Point(
  x: Int,
  y: Int
) {
  def <->(other: Point): Double = Math.sqrt(Math.pow(other.x - x, 2) + Math.pow(other.y - y, 2))
}

sealed abstract class Instruction(center: Point, diameter: Int) {
  def includes(point: Point): Boolean = (point <-> center) <= (diameter / 2.0)
  def result: String
}
object Instruction {
  case class Mow(center: Point, diameter: Int) extends Instruction(center, diameter) {
    override val result: String = "  "
  }
  case class Plant(center: Point, diameter: Int) extends Instruction(center, diameter) {
    override val result: String = "{}"
  }

  val fromInput: Regex = "^((?:PLANT)?)([a-s])([a-y])(0|[1-9][0-9]*)$".r
  def fromMatch(operation: String, x: String, y: String, d: String): Instruction = {
    operation match {
      case "PLANT" => Plant(Point(x.head - 'a', y.head - 'a'), d.toInt)
      case _ => Mow(Point(x.head - 'a', y.head - 'a'), d.toInt)
    }
  }
}

object Solution extends App {
  val instructions = readLine.split(" ").map {
    case Instruction.fromInput(operation, x, y, d) => Instruction.fromMatch(operation, x, y, d)
  }
  val initialField = Seq.fill(25)(Seq.fill(19)("{}"))
  val field = instructions.foldLeft(initialField){(currentField, instruction) =>
    for {
      y <- 0 until 25
    } yield {
      for {
        x <- 0 until 19
      } yield {
        if(instruction.includes(Point(x, y))) {
          instruction.result
        } else {
          currentField(y)(x)
        }
      }
    }
  }
  println(field.map(_.mkString).mkString("\n"))
}