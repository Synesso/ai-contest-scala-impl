package jem

import scala.math.{ceil,sqrt,max,abs}

case class Planet(index: Int, x: Double, y: Double, owner: Owner, size: Int, regen: Int, fleets: Seq[Fleet]) {

  def distanceTo(other: Planet): Int = distanceTo((other.x, other.y))

  def distanceTo(xy: (Double, Double)) = {
    val dx = x - xy._1
    val dy = y - xy._2
    ceil(sqrt(dx * dx + dy * dy)).asInstanceOf[Int]
  }

  def hasSurplus = Me.equals(owner) && size > 2
  def hasDeficit = !Me.equals(owner) && regen > 0

  lazy val projection = {
    def expandForward(futures: List[Planet]): List[Planet] = {
      val planet = futures.head
      if (planet.fleets.size == 0) futures else {
        val popBeforeFleets = if (planet.owner.equals(Nobody)) planet.size else planet.size + planet.regen
        val (fleetsArriving, fleetsTravelling) = planet.fleets.partition(_.turnsRemaining <= 1)
        val nettedArrivingFleet = fleetsArriving.foldLeft(Fleet(Nobody, 0, index, 0)) {(acc, next) =>
          if (acc.owner.equals(next.owner)) acc.copy(size = acc.size + next.size)
          else {
            if (acc.size > next.size) acc.copy(size = acc.size - next.size)
            else next.copy(size = next.size - acc.size)
          }
        }
        val population = if (nettedArrivingFleet.owner.equals(owner)) {
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

    val futures = expandForward(List(this)).reverse
    val worst = futures.zipWithIndex.reduceLeft{(pi1, pi2) =>
      (pi1._1.owner, pi1._1.size, pi2._1.owner, pi2._1.size) match {
        case (Me, x, Me, y) => if (x > y) pi2 else pi1
        case (Me, _, _, _) => pi2
        case (_, _, Me, _) => pi1
        case (_, x, _, y) => if (x > y) pi1 else pi2
      }
    }

    Projection(this, worst, (futures.last, futures.size - 1))
  }

  lazy val leastFavourableProjection = {
    val (fleetsByTurnsRemaining, maxTurns) = fleets.toList.foldLeft((Map.empty[Int, Set[Fleet]], 0)) {(mapAndMax, fleet) =>
      val (map, maxTurn) = mapAndMax
      val newMap = map.updated(fleet.turnsRemaining, map.getOrElse(fleet.turnsRemaining, Set()) + fleet)
      val newMaxTurn = max(maxTurn, fleet.turnsRemaining)
      (newMap, newMaxTurn)
    }
    def planetAtTurn(turn: Int, thisTurnPlanet: Planet, worstSoFar: Planet): Planet = {
      if (turn > maxTurns) thisTurnPlanet
      else {
        val popBeforeFleets = if (thisTurnPlanet.owner.equals(Nobody)) thisTurnPlanet.size else thisTurnPlanet.size + thisTurnPlanet.regen
        val maybeFleets = fleetsByTurnsRemaining.get(turn)
        val maybeWinningFleet = maybeFleets.map{fleets =>
          fleets.reduceLeft{(f1, f2) =>
            if (f1.owner.equals(f2.owner)) f1.copy(size = f1.size + f2.size)
            else {
              if (f1.size > f2.size) f1.copy(size = f1.size - f2.size)
              else f2.copy(size = f2.size - f1.size)
            }
          }
        }
        val maybePlanet = maybeWinningFleet.map{winningFleet =>
          if (winningFleet.size < popBeforeFleets) thisTurnPlanet.copy(size = popBeforeFleets - winningFleet.size)
          else thisTurnPlanet.copy(size = winningFleet.size - popBeforeFleets, owner = winningFleet.owner)
        }
        val nextTurnPlanet = maybePlanet.getOrElse(thisTurnPlanet.copy(size = popBeforeFleets))
        val worstSoFar = thisTurnPlanet.owner match {
          case Me => if (nextTurnPlanet.owner.equals(Him) || nextTurnPlanet.size < thisTurnPlanet.size) nextTurnPlanet else thisTurnPlanet
          case Him => if (nextTurnPlanet.owner.equals(Him) && nextTurnPlanet.size > thisTurnPlanet.size) nextTurnPlanet else thisTurnPlanet
          case Nobody => if (nextTurnPlanet.owner.equals(Nobody) && nextTurnPlanet.size == thisTurnPlanet.size) thisTurnPlanet else nextTurnPlanet
        }
        planetAtTurn(turn + 1, nextTurnPlanet, worstSoFar)
      }
    }
    planetAtTurn(1, this, this)
  }
}