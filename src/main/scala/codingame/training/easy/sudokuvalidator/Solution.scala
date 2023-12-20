package codingame.training.easy.sudokuvalidator

import scala.io.StdIn._

object Solution extends App {
  def hasRowError(grid: Seq[Seq[String]]): Boolean = grid.exists(
    _.groupMapReduce(identity[String])(_ => 1)(_ + _).exists(_._2 > 1)
  )

  def hasColumnError(grid: Seq[Seq[String]]): Boolean =
    grid
      .flatMap(_.zipWithIndex)
      .groupMapReduce(identity[(String, Int)])(_ => 1)(_ + _)
      .exists(_._2 > 1)

  def hasSubgridError(grid: Seq[Seq[String]]): Boolean =
    grid
      .map(_.zipWithIndex)
      .zipWithIndex
      .flatMap(line => line._1.map {
        case (value, index) => (value, index, line._2)
      })
      .groupBy {
        case (_, line, row) => (line / 3, row / 3)
      }
      .view
      .mapValues(_.map(_._1).groupMapReduce(identity[String])(_ => 1)(_ + _).exists(_._2 > 1))
      .exists(_._2)

  val grid = (for {
    _ <- 0 until 9
  } yield {
    readLine
  })
    .map(_.split(" ").toSeq)

  println(!hasRowError(grid) && !hasColumnError(grid) && !hasSubgridError(grid))
}
