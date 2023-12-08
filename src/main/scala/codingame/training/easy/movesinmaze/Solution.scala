package codingame.training.easy.movesinmaze

import codingame.training.easy.movesinmaze.Solution.Height
import codingame.training.easy.movesinmaze.Space._

import scala.io.StdIn._
import scala.language.implicitConversions

sealed trait Space
object Space {
  case object Free extends Space
  case object Wall extends Space
  case object StartingPosition extends Space
}

case class Maze(spaces: Seq[Seq[Space]])
trait StartingPosition { self => Maze
  def startingPosition: (Int, Int)
}


object Solution extends App {
  case class Width(value: Int) extends AnyVal
  implicit def widthToInt(width: Width): Int = width.value

  case class Height(value: Int) extends AnyVal
  implicit def heightToInt(height: Height): Int = height.value
  
  implicit class Coordinate(value: Int) {
    def north(implicit height: Height): Int = Some(value - 1).filterNot(_ < 0).getOrElse(height - 1)
    def south(implicit height: Height): Int = Some(value + 1).filterNot(_ >= height).getOrElse(0)
    def west(implicit width: Width): Int = Some(value - 1).filterNot(_ < 0).getOrElse(width - 1)
    def east(implicit width: Width): Int = Some(value + 1).filterNot(_ >= width).getOrElse(0)
  }

  case class Point(
                    x: Int,
                    y: Int
                  ) {
    def north(implicit height: Height): Point = Point(x, y.north)
    def south(implicit height: Height): Point = Point(x, y.south)
    def west(implicit width: Width): Point = Point(x.west, y)
    def east(implicit width: Width): Point = Point(x.east, y)
  }
  trait Reached { self: Point =>
    def moves: Int
  }
  val Array(w, h) = readLine.split(" ")
    .filterNot(_.isEmpty)
    .map(_.toInt)
  implicit val height: Height = Height(h)
  implicit val width: Width = Width(w)
  
  val maze = LazyList.fill(height)(readLine.toSeq)
  val startingPositionY = maze.indexWhere(_.exists(_ == 'S'))
  val startingPositionX = maze(startingPositionY).indexWhere(_ == 'S')
  
  val output = maze.map(_.map {
    case 'S' => '0'
    case '.' => '1'
    case anythingElse => anythingElse
  }.mkString).mkString("\n")
  println(output)
}