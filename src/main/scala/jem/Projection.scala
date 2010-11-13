package jem

import scala.math.{min, max}

case class Projection(current: Planet, worst: (Planet, Int), last: (Planet, Int)) {
  val regen = current.regen
  val surplus = (current.owner, current.size, worst._1.owner, worst._1.size) match {
    case (Me, x, Me, y) => min(x, y) - 1
    case (Me, x, _, y) => y * -1 - 1
    case (_, x, Me, y) => 0
    case (_, x, _, y) => max(x, y) * -1 - 1
  }
  def distanceTo(xy: (Double, Double)) = current.distanceTo(xy)
  def distanceTo(planet: Planet) = current.distanceTo(planet)
  def distanceTo(other: Projection) = current.distanceTo(other.current)
  val x = current.x
  val y = current.y

  def afterSending(amount: Int) = Projection(current.afterSending(amount), (worst._1.afterSending(amount), worst._2),
    (last._1.afterSending(amount), last._2))

  def withMyReinforcements(reinforcementSize: Int, turnsToArrive: Int) = {
    def newFuturePlanet(pi: (Planet, Int)) = {
      val (planet, turnsAhead) = pi
      if (turnsAhead < turnsToArrive) pi
      else {
        if (planet.owner.equals(Me)) (planet.copy(size = planet.size + reinforcementSize), turnsAhead)
        else {
          val newSize = planet.size - reinforcementSize
          if (newSize > 0) (planet.copy(size = newSize), turnsAhead)
          else (planet.copy(size = newSize * -1, owner = Me), turnsAhead)
        }
      }
    }
    Projection(current, newFuturePlanet(worst), newFuturePlanet(last))
  }

}