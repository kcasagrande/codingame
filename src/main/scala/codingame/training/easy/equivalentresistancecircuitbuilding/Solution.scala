package codingame.training.easy.equivalentresistancecircuitbuilding

import java.util.Locale.US
import scala.io.StdIn._
import scala.util.matching.Regex
import scala.collection.immutable.Map

sealed trait Circuit {
  def resistance(individualResistances: Map[String, Float]): Float
}
object Circuit {
  private val regex: Regex = Seq(Resistor.regex.regex, Serie.regex.regex, Parallel.regex.regex).mkString("|").r
  def apply(circuitAsString: String): Circuit = circuitAsString match {
    case Resistor.regex(name) => Resistor(name)
    case Serie.regex(subCircuit) => Serie(regex.findAllMatchIn(subCircuit).toSeq.map(_.matched).map(Circuit(_)))
    case Parallel.regex(subCircuit) => Parallel(regex.findAllMatchIn(subCircuit).toSeq.map(_.matched).map(Circuit(_)))
  }

  case class Resistor(name: String) extends Circuit {
    override def resistance(individualResistances: Map[String, Float]): Float = individualResistances(name)
  }
  object Resistor {
    val regex: Regex = """([^\s\)\(\]\[]+)""".r
  }
  case class Serie(subCircuits: Seq[Circuit]) extends Circuit {
    override def resistance(individualResistances: Map[String, Float]): Float = subCircuits.map(_.resistance(individualResistances)).sum
  }
  object Serie {
    val regex: Regex = """\( (.*) \)""".r
  }
  case class Parallel(subCircuits: Seq[Circuit]) extends Circuit {
    override def resistance(individualResistances: Map[String, Float]): Float = 1 / subCircuits.map(_.resistance(individualResistances)).map(1/_).sum
  }
  object Parallel {
    val regex: Regex = """\[ (.*) \]""".r
  }
}

object Solution extends App {
  val resistorsNumber = readLine.toInt
  val resistances: Map[String, Float] =
    (
      for {
        _ <- 0 until resistorsNumber
      } yield {
        readLine
      }
    )
      .map(_.split(" "))
      .map(array => array.head -> array.last.toFloat)
      .toMap
  val circuit = Circuit(readLine)
  println("%.1f".formatLocal(US, circuit.resistance(resistances)))
}