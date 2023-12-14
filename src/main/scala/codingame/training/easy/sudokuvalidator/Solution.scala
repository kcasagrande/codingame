package codingame.training.easy.sudokuvalidator

import scala.io.StdIn._

object Solution extends App {
  def hasRowError(grid: Seq[Seq[String]]): Boolean = grid.exists(
    _.containsSlice(Seq("2", "2"))
  )

  val grid = (for {
    _ <- 0 until 9
  } yield {
    readLine
  })
    .map(_.split(" ").toSeq)

  println(!hasRowError(grid))
}
