package codingame.training.easy.logicgates

import scala.io.StdIn._

sealed abstract class Level(val symbol: Char) {
  override def toString: String = symbol.toString
  def inverted: Level
}
object Level {
  case object Low extends Level('_') {
    override def inverted: Level = High
  }
  case object High extends Level('-') {
    override def inverted: Level = Level.Low
  }
  def apply(representation: Char): Level = {
    representation match {
      case '-' => High
      case '_' => Low
    }
  }
}

case class Signal(levels: Seq[Level]) {
  override def toString: String = levels.mkString
}
object Signal {
  def fromString(representation: String): Signal = Signal(representation.map(Level(_)))
}

sealed abstract class Gate(val combine: (Level, Level) => Level) extends ((Signal, Signal) => Signal) {
  override def apply(in1: Signal, in2: Signal): Signal =
    Signal(in1.levels.zip(in2.levels).map(combine.tupled))
}
object Gate {
  case object And extends Gate((a, b) => a match {
    case Level.High => b
    case _ => Level.Low
  })

  case object Or extends Gate((a, b) => a match {
    case Level.High => Level.High
    case _ => b
  })

  case object Xor extends Gate({
    case (Level.High, Level.Low) | (Level.Low, Level.High) => Level.High
    case _ => Level.Low
  })

  case object Nor extends Gate(Or.combine(_, _).inverted)
  case object Nand extends Gate(And.combine(_, _).inverted)
  case object Nxor extends Gate(Xor.combine(_, _).inverted)

  def fromString(representation: String): Gate = representation match {
    case "AND" => And
    case "NAND" => Nand
    case "NOR" => Nor
    case "NXOR" => Nxor
    case "OR" => Or
    case "XOR" => Xor
  }
}

object Solution extends App {
  private val n = readLine.toInt
  private val m = readLine.toInt

  private val inputSignals = LazyList.fill(n)(readLine())
    .map(_.split(" "))
    .map {
      case Array(name, signal) => name -> signal
    }
    .toMap

  LazyList.fill(m)(readLine())
    .map(_.split(" "))
    .foreach {
      case Array(outputName, _type, inputName1, inputName2) =>
        val gate = Gate.fromString(_type)
        val in1 = Signal.fromString(inputSignals(inputName1))
        val in2 = Signal.fromString(inputSignals(inputName2))
        val output = gate(in1, in2)
        println(s"$outputName $output")
    }
}
