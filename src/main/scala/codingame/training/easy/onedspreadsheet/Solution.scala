package codingame.training.easy.onedspreadsheet

import scala.io.StdIn._

case class WithCache[T](t: T, cache: Map[Int, Int])

sealed trait Operand {
  def computeValue(cache: Map[Int, Int], operations: Seq[Operation]): WithCache[Int]
}
object Operand {
  def apply(token: String): Operand = {
    token match {
      case Value.regex(value) => Value(value.toInt)
      case Reference.regex(cell) => Reference(cell.toInt)
    }
  }
  final case class Value(private val value: Int) extends Operand {
    def computeValue(cache: Map[Int, Int], operations: Seq[Operation]): WithCache[Int] =
      WithCache(value, cache)
  }
  object Value {
    val regex = "^(0|-?[1-9][0-9]*)$".r
  }
  final case class Reference(cell: Int) extends Operand {
    def computeValue(cache: Map[Int, Int], operations: Seq[Operation]): WithCache[Int] = {
      cache.get(cell).map(WithCache(_, cache)).getOrElse {
        val result = operations(cell).computeValue(cache, operations)
        WithCache(result.t, result.cache + (cell -> result.t))
      }
    }
  }
  object Reference {
    val regex = "^\\$(0|[1-9][0-9]*)$".r
  }
}

sealed trait Operation {
  def index: Int
  def computeValue(cache: Map[Int, Int], operations: Seq[Operation]): WithCache[Int]
}
object Operation {
  final case class Value(index: Int, operand: Operand) extends Operation {
    override def computeValue(cache: Map[Int, Int], operations: Seq[Operation]): WithCache[Int] = {
      val result = operand.computeValue(cache, operations)
      WithCache(result.t, result.cache + (index -> result.t))
    }
  }
  final case class Add(index: Int, operand1: Operand, operand2: Operand) extends Operation {
    override def computeValue(cache: Map[Int, Int], operations: Seq[Operation]): WithCache[Int] = {
      val result1 = operand1.computeValue(cache, operations)
      val result2 = operand2.computeValue(result1.cache, operations)
      val result = result1.t + result2.t
      WithCache(result, result2.cache + (index -> result))
    }
  }
  final case class Sub(index: Int, operand1: Operand, operand2: Operand) extends Operation {
    override def computeValue(cache: Map[Int, Int], operations: Seq[Operation]): WithCache[Int] = {
      val result1 = operand1.computeValue(cache, operations)
      val result2 = operand2.computeValue(result1.cache, operations)
      val result = result1.t - result2.t
      WithCache(result, result2.cache + (index -> result))
    }
  }
  final case class Mult(index: Int, operand1: Operand, operand2: Operand) extends Operation {
    override def computeValue(cache: Map[Int, Int], operations: Seq[Operation]): WithCache[Int] = {
      val result1 = operand1.computeValue(cache, operations)
      val result2 = operand2.computeValue(result1.cache, operations)
      val result = result1.t * result2.t
      WithCache(result, result2.cache + (index -> result))
    }
  }

  def parse(index: Int, tokens: Seq[String]): Operation = tokens match {
    case "VALUE" :: operand :: _ => Operation.Value(index, Operand(operand))
    case "ADD" :: operand1 :: operand2 :: _ => Operation.Add(index, Operand(operand1), Operand(operand2))
    case "SUB" :: operand1 :: operand2 :: _ => Operation.Sub(index, Operand(operand1), Operand(operand2))
    case "MULT" :: operand1 :: operand2 :: _ => Operation.Mult(index, Operand(operand1), Operand(operand2))
  }
}

object Solution extends App {
  val n = readLine.toInt
  val operations: Seq[Operation] =
    LazyList.fill(n)(readLine)
      .map(_.split(" ").toList)
      .zipWithIndex
      .map {
        case (tokens, index) => Operation.parse(index, tokens)
      }
  val cache = Map.empty[Int, Int]
  operations
    .foldLeft(cache)((currentCache, operation) => operation.computeValue(currentCache, operations).cache)
    .toSeq
    .sortBy(_._1)
    .map(_._2)
    .foreach(println)
}
