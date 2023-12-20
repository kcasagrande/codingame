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

  case class Point(x: Int, y: Int)

  case class Drone(id: Id, position: Point, battery: Int)

  case class Contestant(score: Int = 0, scans: Seq[Id] = Seq.empty[Id], drones: Set[Drone] = Set.empty[Drone])

  case class TurnContext(player: Contestant, foe: Contestant, creatures: Map[Id, (Point, Point)])

  def readTurnContext(): TurnContext = {

    def readScans: Seq[Id] = Seq.fill(readLine.toInt) {
      readLine
    }

    def readDrones: Set[Drone] = Set.fill(readLine.toInt) {
      val Array(droneId, droneX, droneY, emergency, battery) = (readLine split " ").filter(_ != "")
      Drone(droneId, Point(droneX.toInt, droneY.toInt), battery.toInt)
    }

    val playerScore = readLine.toInt
    val foeScore = readLine.toInt

    val playerScans = readScans
    val foeScans = readScans

    val playerDrones = readDrones
    val foeDrones = readDrones

    val droneScanCount = readLine.toInt
    for(i <- 0 until droneScanCount) {
      val Array(droneId, creatureId) = (readLine split " ").filter(_ != "").map (_.toInt)
    }

    val visibleCreatures = Seq.fill(readLine.toInt){
      val Array(creatureId, creatureX, creatureY, creatureVx, creatureVy) = (readLine split " ").filter(_ != "")
      creatureId -> (Point(creatureX.toInt, creatureY.toInt), Point(creatureVx.toInt, creatureVy.toInt))
    }.toMap

    val radarBlipCount = readLine.toInt
    for(i <- 0 until radarBlipCount) {
      val Array(_droneId, _creatureId, radar) = readLine split " "
      val droneId = _droneId.toInt
      val creatureId = _creatureId.toInt
    }

    TurnContext(
      Contestant(playerScore, playerScans, playerDrones),
      Contestant(foeScore, foeScans, foeDrones),
      visibleCreatures
    )
  }

  val gameContext: GameContext = {
    val creatureCount = readLine.toInt
    GameContext(
      Set.fill(creatureCount) {
        val Array(creatureId, color, _type) = (readLine split " ").filter(_ != "")
        Creature(creatureId, color.toInt, _type.toInt)
      }
    )
  }

  LazyList.continually(
    readTurnContext()
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
