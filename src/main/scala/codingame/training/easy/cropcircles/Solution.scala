package codingame.training.easy.cropcircles

object Solution extends App {
  val field = (for {
    y <- 0 until 25
  } yield {
    (for {
      x <- 0 until 19
    } yield {
      if(x == 'o' - 'a' && y == 'e' - 'a') {
        "  "
      } else {
        "{}"
      }
    }).mkString
  }).mkString("\n")
  println(field)
}