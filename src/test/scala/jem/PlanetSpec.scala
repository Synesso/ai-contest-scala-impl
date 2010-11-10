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

  "my planet that will not be taken by an enemy fleet" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 5, List(Fleet(Him, 33, -1, 2)))

    "have the final state as the worst state" in {
      val endState = planet.copy(size = 77, owner = Me, fleets = Nil)
      planet.projection must_== Projection(planet, (endState, 2), (endState, 2))
    }
  }

  "my planet that will have more regen than enemy fleet size" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 15, List(Fleet(Him, 20, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 110, owner = Me, fleets = Nil)
      planet.projection must_== Projection(planet, (planet, 0), (endState, 2))
    }
  }

  "my planet that will receive re-inforcements" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 5, List(Fleet(Me, 125, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 235, owner = Me, fleets = Nil)
      planet.projection must_== Projection(planet, (planet, 0), (endState, 2))
    }
  }

  "neutral planet that exepects no fleets" should {
    val planet = Planet(1, 1.0, 1.0, Nobody, 50, 5, Nil)

    "have the current state for all three phases of the projection" in {
      val endState = planet.copy(size = 15, owner = Nobody, fleets = Nil)
      planet.projection must_== Projection(planet, (planet, 0), (planet, 0))
    }
  }

  "neutral planet that will not be taken by my fleets" should {
    val planet = Planet(1, 1.0, 1.0, Nobody, 50, 5, List(Fleet(Me, 35, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 15, owner = Nobody, fleets = Nil)
      planet.projection must_== Projection(planet, (planet, 0), (endState, 2))
    }
  }

  "neutral planet that will be taken by my fleets" should {
    val planet = Planet(1, 1.0, 1.0, Nobody, 50, 5, List(Fleet(Me, 85, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 35, owner = Me, fleets = Nil)
      planet.projection must_== Projection(planet, (planet, 0), (endState, 2))
    }
  }


}