package codingame.training.easy.encryptiondecryptionofenigmamachine

import scala.io.StdIn._

sealed trait Operation extends (String => String) {
  override def apply(message: String): String
}
object Operation {
  private def caesarShift(shift: Int)(implicit alphabet: String): (String) => String = _
    .map(alphabet.indexOf(_))
    .zipWithIndex
    .map(tuple => tuple._1 + tuple._2)
    .map(_ + shift)
    .map(_ % alphabet.length)
    .map(alphabet(_))
    .mkString

  private def caesarUnshift(shift: Int)(implicit alphabet: String): (String) => String = caesarShift(shift)(alphabet.reverse)

  case class Encode(private val initialShift: Int, private val rotors: Seq[Rotor])(implicit alphabet: String) extends Operation {
    override def apply(message: String): String = rotors.foldLeft(caesarShift(initialShift)){ (operations, rotor) => operations andThen rotor }(message)
  }

  case class Decode(private val initialShift: Int, private val rotors: Seq[Rotor])(implicit alphabet: String) extends Operation {
    override def apply(message: String): String = (rotors.foldRight(identity[String](_)) { (rotor, operations ) => operations andThen rotor.reverse } andThen caesarUnshift(initialShift))(message)
  }

  def apply(operationAsString: String, initialShift: Int, rotors: Seq[Rotor])(implicit alphabet: String): Operation = {
    operationAsString match {
      case "ENCODE" => Encode(initialShift, rotors)
      case "DECODE" => Decode(initialShift, rotors)
    }
  }
}


case class Rotor(configuration: String)(implicit alphabet: String) extends (String => String) {
  override def apply(message: String): String =
    message
      .map(alphabet.indexOf(_))
      .map(configuration(_))
      .mkString

  val reverse: String => String = _
    .map(configuration.indexOf(_))
    .map(alphabet(_))
    .mkString
}

object Solution extends App {
  implicit val alphabet: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  val operation = readLine()
  val initialShift = readLine().toInt
  val rotors = Seq.fill(3)(readLine())
    .map(Rotor(_))
  val message = readLine()

  println(Operation(operation, initialShift, rotors).apply(message))
}