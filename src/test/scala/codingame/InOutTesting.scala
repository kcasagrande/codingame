package codingame

import org.scalatest.TestSuite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

trait InOutTesting { _: TestSuite =>
  def outputWithInput(input: String*)(test: String => Any)(implicit app: App) = {
    val inputStream = new ByteArrayInputStream(input.mkString("\n").getBytes)
    val outputStream = new ByteArrayOutputStream()
    Console.withIn(inputStream) {
      Console.withOut(outputStream) {
        app.main(Array.empty[String])
      }
    }
    test(outputStream.toString)
  }
}
