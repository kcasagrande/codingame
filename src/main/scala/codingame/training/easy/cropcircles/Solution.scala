package codingame.training.easy.cropcircles

import codingame.training.easy.cropcircles.State.{Mowed, Planted}

import scala.io.StdIn._
import scala.util.matching.Regex

case class Point(
  x: Int,
  y: Int
) {
  def <->(other: Point): Double = Math.sqrt(Math.pow(other.x - x, 2) + Math.pow(other.y - y, 2))
}

sealed trait State extends (Instruction => State) {
  final override def apply(instruction: Instruction): State = instruction match {
    case Instruction.Plant(_, _) => Planted
    case Instruction.Mow(_, _) => Mowed
    case Instruction.PlantMow(_, _) => plantMow
  }
  protected def plantMow: State
}
object State {
  case object Planted extends State {
    override def toString: String = "{}"
    override protected def plantMow: State = Mowed
  }
  case object Mowed extends State {
    override def toString: String = "  "
    override protected def plantMow: State = Planted
  }
}

sealed abstract class Instruction(center: Point, diameter: Int) {
  def includes(point: Point): Boolean = (point <-> center) <= (diameter / 2.0)
}
object Instruction {
  case class Mow(center: Point, diameter: Int) extends Instruction(center, diameter)
  case class Plant(center: Point, diameter: Int) extends Instruction(center, diameter)
  case class PlantMow(center: Point, diameter: Int) extends Instruction(center, diameter)

  val fromInput: Regex = "^((?:PLANT)?|(?:PLANTMOW)?)([a-s])([a-y])(0|[1-9][0-9]*)$".r
  def fromMatch(operation: String, x: String, y: String, d: String): Instruction = {
    operation match {
      case "PLANT" => Plant(Point(x.head - 'a', y.head - 'a'), d.toInt)
      case "PLANTMOW" => PlantMow(Point(x.head - 'a', y.head - 'a'), d.toInt)
      case _ => Mow(Point(x.head - 'a', y.head - 'a'), d.toInt)
    }
  }
}

object Solution extends App {
  private val width = 19
  private val height = 25
  val instructions = readLine.split(" ").map {
    case Instruction.fromInput(operation, x, y, d) => Instruction.fromMatch(operation, x, y, d)
  }
  val initialField: Seq[Seq[State]] = Seq.fill(height)(Seq.fill(width)(State.Planted))
  val field = instructions.foldLeft(initialField){(currentField, instruction) =>
    for {
      y <- 0 until height
    } yield {
      for {
        x <- 0 until width
      } yield {
        if(instruction.includes(Point(x, y))) {
          currentField(y)(x).apply(instruction)
        } else {
          currentField(y)(x)
        }
      }
    }
  }
  println(field.map(_.map(_.toString).mkString).mkString("\n"))
}