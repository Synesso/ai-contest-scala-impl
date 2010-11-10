package jem

import org.specs.Specification

class PlanetSpec extends Specification {

  "a planet with no incoming fleets" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 5, Nil)

    "have the current state for all three phases of the projection" in {
      planet.projection must_== Projection(planet, (planet, 0), (planet, 0))
    }
  }

  "my planet that will be taken by an enemy fleet" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 5, List(Fleet(Him, 125, -1, 2)))

    "have the final state as the worst state" in {
      val endState = planet.copy(size = 15, owner = Him, fleets = Nil)
      planet.projection must_== Projection(planet, (endState, 2), (endState, 2))
    }
  }


}