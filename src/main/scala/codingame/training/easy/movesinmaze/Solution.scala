package codingame.training.easy.movesinmaze

import scala.collection.immutable
import scala.io.StdIn._
import scala.language.implicitConversions

sealed trait Space
object Space {
  case object Free extends Space
  case object Wall extends Space
  case object StartingPosition extends Space
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
    
    def isWall(implicit maze: Maze): Boolean = maze(y)(x) == Space.Wall
  }
  val Array(w, h) = readLine.split(" ")
    .filterNot(_.isEmpty)
    .map(_.toInt)
  implicit val height: Height = Height(h)
  implicit val width: Width = Width(w)

  type Maze = Seq[Seq[Space]]
  implicit val maze: Maze = LazyList.fill(height)(readLine.map {
    case 'S' => Space.StartingPosition
    case '.' => Space.Free
    case _ => Space.Wall
  })
  
  val startingPositionY = maze.indexWhere(_.exists(_ == Space.StartingPosition))
  val startingPositionX = maze(startingPositionY).indexWhere(_ == Space.StartingPosition)
  
  def run(
           from: Point,
           fromDistance: Int,
           distances: immutable.Map[Point, Int] = immutable.Map.empty[Point, Int]
         )(implicit maze: Maze): immutable.Map[Point, Int] = {
    def next(point: Point): immutable.Map[Point, Int] =
      Some(point).filterNot(_.isWall).map(run(_, fromDistance + 1, distances + (from -> fromDistance))).getOrElse(immutable.Map.empty[Point, Int])
    distances.get(from).filter(_ < fromDistance)
      match {
        case None => distances + (from -> fromDistance) ++
          next(from.north) ++
          next(from.south) ++
          next(from.west) ++
          next(from.east)
        case _ => distances
      }
  }

  val distances = run(Point(startingPositionX, startingPositionY), 0)
  
  val digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  
  val output = (for {
    y <- 0 until height
  } yield {
    (for {
      x <- 0 until width
    } yield {
      distances.get(Point(x, y))
        .map(digits(_))
        .getOrElse(maze(y)(x) match {
          case Space.Wall => '#'
          case Space.Free => '.'
        })
    })
      .mkString
  })
    .mkString("\n")
  println(output)
}