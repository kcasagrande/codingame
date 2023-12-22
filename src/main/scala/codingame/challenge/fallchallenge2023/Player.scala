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

  case class Creature(color: Int, `type`: Int) {
    val points: Int = `type` + 1
  }

  case class GameContext(creatures: Map[Id, Creature])

  trait BiDimensionalMeasurement {
    def x: Int
    def y: Int
  }

  implicit class NumericOps[N : Numeric](value : N) {
    def ^(exponent: Double): Double = pow(implicitly[Numeric[N]].toDouble(value), exponent)
  }

  case class Point(x: Int, y: Int) extends BiDimensionalMeasurement {
    def <->(other: Point): Double = sqrt(pow(other.x - x, 2.0) + pow(other.y - y, 2.0))
    def angleWith(b: Point, c: Point): Double = {
      val ab = this <-> b
      val ac = this <-> c
      val bc = b <-> c
      acos((ab^2 + ac^2 - bc^2) / (2 * ac * ab))
    }
  }

  case class Motion(x: Int, y: Int) extends BiDimensionalMeasurement

  case class Drone(id: Id, position: Point, battery: Int)

  case class Contestant(score: Int = 0, scans: Seq[Id] = Seq.empty[Id], drones: Set[Drone] = Set.empty[Drone])

  case class TurnContext(player: Contestant, foe: Contestant, creatures: Map[Id, (Point, Motion)])

  sealed abstract class Action(val light: Boolean)
  object Action {
    case class Wait(override val light: Boolean = false) extends Action(light) {
      override def toString: String = s"WAIT ${if(light) 1 else 0}"
    }
    case class Move(destination: Point, override val light: Boolean = false) extends Action(light) {
      override def toString: String = s"MOVE ${destination.x} ${destination.y} ${if(light) 1 else 0}"
    }
  }

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
      creatureId -> (Point(creatureX.toInt, creatureY.toInt), Motion(creatureVx.toInt, creatureVy.toInt))
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
      Seq.fill(creatureCount) {
        val Array(creatureId, color, _type) = (readLine split " ").filter(_ != "")
        creatureId -> Creature(color.toInt, _type.toInt)
      }
        .toMap
    )
  }

  def actions(gameContext: GameContext, contexts: TurnContext*): Map[Id, Action] = {

    def distancesMatrix(context: TurnContext): Map[Id, Map[Id, Double]] =
      context.player.drones.map { drone =>
        drone.id -> context.creatures.map {
          case (id, (position, _)) => id -> (drone.position <-> position)
        }
      }
        .toMap

    val distances = distancesMatrix(contexts.last)

    distances.view.mapValues(_.toSeq.minBy(_._2))

    /*
    1. Filtrer les créatures déjà scannées.
    2. S'il existe des créatures non scannées entre 800 et 2000, activer la lumière.
    3. Aller en direction de la créature non scannée à plus de 2000 qui possède le meilleur ratio
    de Points rapportés / Distance.
     */

    contexts.last.player.drones.map { drone =>
      drone.id -> Action.Wait(contexts.size > 1)
    }
      .toMap
  }

  LazyList.continually(
    readTurnContext()
  )
    .scanLeft(Seq.empty[TurnContext])(_ :+ _)
    .tail
    .map(_.takeRight(1 + lookback))
    .map(turn => {
      Console.err.println(turn)
      turn
    })
    .map(actions(gameContext, _:_*))
    .map(_.head._2)
    .foreach(println)

}
