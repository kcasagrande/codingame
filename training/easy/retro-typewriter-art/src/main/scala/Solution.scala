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
            case characterChunk(quantity, character) => abbreviations.applyOrElse[String, String](character, identity).repeat(quantity.toInt)
        }
        .foreach(print)
}
