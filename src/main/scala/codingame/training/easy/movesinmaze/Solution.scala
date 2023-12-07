package codingame.training.easy.movesinmaze

import scala.io.StdIn._

object Solution extends App {
  val Array(width, height) = readLine.split(" ").filterNot(_.isEmpty).map(_.toInt)
  val maze = LazyList.fill(height)(readLine.toSeq)
  val output = maze.map(_.map {
    case 'S' => '0'
    case '.' => '1'
    case anythingElse => anythingElse
  }.mkString).mkString("\n")
  println(output)
}