package codingame.training.easy.sudokuvalidator

import scala.io.StdIn._

object Solution extends App {
  val grid = for {
    _ <- 0 until 9
  } yield {
    readLine
  }
  println(!grid.exists(_.contains("2 2")))
}
