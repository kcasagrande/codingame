package codingame.challenge.fallchallenge2023

import codingame.challenge.fallchallenge2023.Player.Point
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class PointTest
  extends AnyFreeSpecLike
{
  "Point.<->" - {
    "should compute the correct distance" in {
      val a = Point(0, 0)
      val b = Point(3, 4)
      ((a <-> b) - 5.0) should be <= 0.01
    }
  }

  "Point.angleWith" - {
    "should compute the correct angle" in {
      val a = Point(0, 0)
      val b = Point(3, 0)
      val c = Point(3, 4)
      a.angleWith(b, c) - 30.0 should be <= 0.01
    }
    "should compute the correct opposite angle" in {
      val a = Point(0, 0)
      val b = Point(3, 0)
      val c = Point(3, 4)
      a.angleWith(c, b) - 30.0 should be <= 0.01
    }
  }
}
