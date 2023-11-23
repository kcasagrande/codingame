package codingame.training.easy.retrotypewriterart

import scala.io.StdIn

object Solution extends App {
    val abbreviations = Map(
        "sp" -> " ",
        "bS" -> "\\",
        "sQ" -> "'"
    )
    val characterChunk = s"""^([1-9][0-9]*)(.|${abbreviations.keys.mkString("|")})$$""".r
    StdIn.readLine()
        .split(" ")
        .map {
            case "nl" => "\n"
            case characterChunk(quantity, character) =>
              val actualCharacter = abbreviations.applyOrElse[String, String](character, identity)
              List.fill(quantity.toInt)(actualCharacter).mkString
        }
        .foreach(print)
}
