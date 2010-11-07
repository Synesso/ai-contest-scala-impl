package jem

import org.specs.Specification

class PlanetSpec extends Specification {

  "a planet with no incoming fleets" should {
    "find the current state as the worst" in {
      val planet = Planet(1, 1.0, 1.0, Me, 100, 5, Nil)
      planet.leastFavourableProjection must_== planet
    }
  }

  "my planet that will be taken by an enemy fleet" should {
    "find the final state as the worst" in {
      val planet = Planet(1, 1.0, 1.0, Me, 100, 5, List(Fleet(Him, 125, -1, 2)))
      println(planet)
      println(planet.leastFavourableProjection)
      println(planet.copy(owner = Him, size = 15))
      planet.leastFavourableProjection must_== planet.copy(owner = Him, size = 15)
    }
  }

}