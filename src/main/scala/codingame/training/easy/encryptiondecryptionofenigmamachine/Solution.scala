package codingame.training.easy.encryptiondecryptionofenigmamachine

import scala.io.StdIn._

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

  def caesarShift(shift: Int)(implicit alphabet: String): (String) => String = _
    .map(alphabet.indexOf(_))
    .zipWithIndex
    .map(tuple => tuple._1 + tuple._2)
    .map(_ + shift)
    .map(_ % alphabet.length)
    .map(alphabet(_))
    .mkString

  def caesarUnshift(shift: Int)(implicit alphabet: String): (String) => String = caesarShift(shift)(alphabet.reverse)

  val encode = rotors.foldLeft(caesarShift(initialShift)) { (operations, rotor) => operations andThen rotor }
  val decode = rotors.foldRight(identity[String](_)){ (rotor, operations) => operations andThen rotor.reverse } andThen caesarUnshift(initialShift)

  if(operation == "ENCODE") {
    println(encode(message))
  } else {
    println(decode(message))
  }
}