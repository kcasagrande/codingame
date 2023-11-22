import scala.io.StdIn._

sealed trait Operand {
    def value(map: Map[Int, Operation]): Int
}
object Operand {
    def apply(token: String): Operand = {
        token match {
            case Value.regex(value) => Value(value.toInt)
            case Reference.regex(cell) => Reference(cell.toInt)
        }
    }
    final case class Value(private val _value: Int) extends Operand {
        override def value(map: Map[Int, Operation]): Int = _value
    }
    object Value {
        val regex = "^(0|-?[1-9][0-9]*)$".r
    }
    final case class Reference(cell: Int) extends Operand {
        override def value(map: Map[Int, Operation]): Int = map(cell).value(map)
    }
    object Reference {
        val regex = "^\\$(0|[1-9][0-9]*)$".r
    }
}

sealed trait Operation {
    def value(map: Map[Int, Operation]): Int
}
object Operation {
    val operandRegex = "[^\\s]+"

    def apply(line: String): Operation = {
        line match {
            case Value.regex(operand) => Value(Operand(operand))
            case Add.regex(operand1, operand2) => Add(Operand(operand1), Operand(operand2))
            case Sub.regex(operand1, operand2) => Sub(Operand(operand1), Operand(operand2))
            case Mult.regex(operand1, operand2) => Mult(Operand(operand1), Operand(operand2))
        }
    }
    final case class Value(operand: Operand) extends Operation {
        override def value(map: Map[Int, Operation]): Int = operand.value(map)
    }
    object Value {
        val regex = s"^VALUE ($operandRegex) $operandRegex$$".r
    }
    final case class Add(operand1: Operand, operand2: Operand) extends Operation {
        override def value(map: Map[Int, Operation]): Int = operand1.value(map) + operand2.value(map)
    }
    object Add {
        val regex = s"^ADD ($operandRegex) ($operandRegex)$$".r
    }
    final case class Sub(operand1: Operand, operand2: Operand) extends Operation {
        override def value(map: Map[Int, Operation]): Int = operand1.value(map) - operand2.value(map)
    }
    object Sub {
        val regex = s"^SUB ($operandRegex) ($operandRegex)$$".r
    }
    final case class Mult(operand1: Operand, operand2: Operand) extends Operation {
        override def value(map: Map[Int, Operation]): Int = operand1.value(map) * operand2.value(map)
    }
    object Mult {
        val regex = s"^MULT ($operandRegex) ($operandRegex)$$".r
    }
}

object Solution extends App {
    val n = readLine.toInt
    val map: Map[Int, Operation] = LazyList.fill(n)(readLine)
        .zipWithIndex
        .map(_.swap)
        .toMap
        .map(entry => entry._1 -> Operation(entry._2))
    map.values
        .map(_.value(map))
        .foreach(println)
}
