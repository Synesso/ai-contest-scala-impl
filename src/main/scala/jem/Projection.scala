package jem

import scala.math.{abs,min,max}

object Projection {
  def of(planet: Planet) = {
    def expandForward(futures: List[Planet]): List[Planet] = {
      val planet = futures.head
      if (planet.fleets.size == 0) futures else {
        val popBeforeFleets = if (planet.owner.equals(Nobody)) planet.size else planet.size + planet.regen
        val (fleetsArriving, fleetsTravelling) = planet.fleets.partition(_.turnsRemaining <= 1)
        val nettedArrivingFleet = fleetsArriving.foldLeft(Fleet(Nobody, 0, planet.index, 0)) {(acc, next) =>
          if (acc.owner.equals(next.owner)) acc.copy(size = acc.size + next.size)
          else {
            if (acc.size > next.size) acc.copy(size = acc.size - next.size)
            else next.copy(size = next.size - acc.size)
          }
        }
        val population = if (nettedArrivingFleet.owner.equals(planet.owner)) {
          popBeforeFleets + nettedArrivingFleet.size
        } else {
          popBeforeFleets - nettedArrivingFleet.size
        }
        val newFleets = fleetsTravelling.map(f => f.copy(turnsRemaining = f.turnsRemaining - 1))
        val newPlanet = if (population < 0) {
          planet.copy(owner = nettedArrivingFleet.owner, size = abs(population), fleets = newFleets)
        } else {
          planet.copy(size = population, fleets = newFleets)
        }
        expandForward(newPlanet :: futures)
      }
    }

    val futures = expandForward(List(planet)).reverse
    Projection(futures)
  }
}

case class Projection(planets: Seq[Planet]) {
  def apply(i: Int) = planets(i)
  val current = planets.head
  val size = planets.size
  val last = size - 1
  val lastPlanet = planets.last
  val (worstPlanet, worst) = planets.zipWithIndex.reduceLeft{(pi1, pi2) =>
    (pi1._1.owner, pi1._1.size, pi2._1.owner, pi2._1.size) match {
      case (Me, x, Me, y) => if (x > y) pi2 else pi1
      case (Me, _, _, _) => pi2
      case (_, _, Me, _) => pi1
      case (_, x, _, y) => if (x < y) pi2 else pi1
    }
  }
  val transitionFromNeutralToEnemy: Option[Int] = if (planets.head.owner.equals(Nobody)) {
    if (planets.last.owner.equals(Nobody)) None else {
      val firstNotNobody = planets.findIndexOf(!_.owner.equals(Nobody))
      if (planets(firstNotNobody).owner.equals(Him)) Some(firstNotNobody) else None
    }
  } else None
  val surplus = (planets.head.owner, planets.head.size, planets(worst).owner, planets(worst).size) match {
    case (Me, x, Me, y) => min(x, y) - 1
    case (Me, x, _, y) => y * -1 - 1
    case (_, x, Me, y) => 0
    case (_, x, _, y) => max(x, y) * -1 - 1
  }

  def distanceTo(xy: (Double, Double)) = planets.head.distanceTo(xy)
  def distanceTo(planet: Planet) = planets.head.distanceTo(planet)
  def distanceTo(other: Projection) = planets.head.distanceTo(other(0))
  val x = planets.head.x
  val y = planets.head.y
  val regen = planets.head.regen

  def afterSending(amount: Int) = Projection.of(planets(0).copy(size = planets(0).size - amount))
}
