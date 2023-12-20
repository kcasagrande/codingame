package codingame.challenge.fallchallenge2023

import math._
import scala.util._
import scala.io.StdIn._

/**
 * Score points by scanning valuable fish faster than your opponent.
 **/
object Player extends App {
  val lookback: Int = 1

  type Id = String

  case class Creature(id: Id, color: Int, `type`: Int)

  case class GameContext(creatures: Set[Creature])

  class Point(x: Int, y: Int)

  case class Drone(id: Id, position: Point, battery: Int)

  case class Contestant(score: Int = 0, scans: Seq[Id] = Seq.empty[Id])

  case class TurnContext(player: Contestant, foe: Contestant, creatures: Map[Id, (Point, Point)])

  val gameContext: GameContext = {
    val creatureCount = readLine.toInt
    GameContext(
      Set.fill(creatureCount) {
        val Array(creatureId, color, _type) = (readLine split " ").filter(_ != "")
        Creature(creatureId, color.toInt, _type.toInt)
      }
    )
  }

  def readTurnInput(): TurnContext = {
    val myScore = readLine.toInt
    val foeScore = readLine.toInt
    val myScanCount = readLine.toInt
    for(i <- 0 until myScanCount) {
      val creatureId = readLine.toInt
    }
    val foeScanCount = readLine.toInt
    for(i <- 0 until foeScanCount) {
      val creatureId = readLine.toInt
    }
    val myDroneCount = readLine.toInt
    for(i <- 0 until myDroneCount) {
      val Array(droneId, droneX, droneY, emergency, battery) = (readLine split " ").filter(_ != "").map (_.toInt)
    }
    val foeDroneCount = readLine.toInt
    for(i <- 0 until foeDroneCount) {
      val Array(droneId, droneX, droneY, emergency, battery) = (readLine split " ").filter(_ != "").map (_.toInt)
    }
    val droneScanCount = readLine.toInt
    for(i <- 0 until droneScanCount) {
      val Array(droneId, creatureId) = (readLine split " ").filter(_ != "").map (_.toInt)
    }
    val visibleCreatureCount = readLine.toInt
    for(i <- 0 until visibleCreatureCount) {
      val Array(creatureId, creatureX, creatureY, creatureVx, creatureVy) = (readLine split " ").filter(_ != "").map (_.toInt)
    }
    val radarBlipCount = readLine.toInt
    for(i <- 0 until radarBlipCount) {
      val Array(_droneId, _creatureId, radar) = readLine split " "
      val droneId = _droneId.toInt
      val creatureId = _creatureId.toInt
    }

    TurnContext(
      Contestant(myScore),
      Contestant(foeScore),
      Map.empty[Id, (Point, Point)]
    )
  }

  LazyList.continually(
    readTurnInput()
  )
    .scanLeft(Seq.empty[TurnContext])(_ :+ _)
    .tail
    .map(_.takeRight(1 + lookback))
    .map {
      case _ :: Nil => "WAIT 0"
      case _ => "WAIT 1"
    }
    .foreach(println)

}
