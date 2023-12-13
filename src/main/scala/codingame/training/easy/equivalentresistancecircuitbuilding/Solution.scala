package codingame.training.easy.equivalentresistancecircuitbuilding

import java.util.Locale.US
import scala.io.StdIn._

object Solution extends App {
  readLine
  val Array(name, _resistance) = readLine.split(" ")
  val resistances: Map[String, Float] = Map(name -> _resistance.toFloat)
  val resistance = resistances("A")
  println("%.1f".formatLocal(US, resistance))
}