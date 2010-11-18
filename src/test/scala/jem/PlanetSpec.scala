package jem

import org.specs.Specification

class PlanetSpec extends Specification {

  "a planet with no incoming fleets" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 5, Nil)

    "have the current state for all phases of the projection" in {
      planet.projection.size mustBe 1
      planet.projection(0) mustEqual planet
      planet.projection.worst mustEqual 0
      planet.projection.worstPlanet mustEqual planet
      planet.projection.last mustEqual 0
      planet.projection.lastPlanet mustEqual planet
      planet.projection.transitionFromNeutralToEnemy must beNone
    }
  }

  "my planet that will be taken by an enemy fleet" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 5, List(Fleet(Him, 125, -1, 2)))

    "have the final state as the worst state" in {
      val endState = planet.copy(size = 15, owner = Him, fleets = Nil)
      planet.projection.size mustBe 3
      planet.projection(0) mustEqual planet
      planet.projection(1) mustEqual planet.copy(size = 105, fleets=List(Fleet(Him, 125, -1, 1)))
      planet.projection(2) mustEqual endState
      planet.projection.worst mustEqual 2
      planet.projection.worstPlanet mustEqual endState
      planet.projection.last mustEqual 2
      planet.projection.lastPlanet mustEqual endState
      planet.projection.transitionFromNeutralToEnemy must beNone
    }
  }

  "my planet that will not be taken by an enemy fleet" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 5, List(Fleet(Him, 33, -1, 2)))

    "have the final state as the worst state" in {
      val endState = planet.copy(size = 77, owner = Me, fleets = Nil)
      planet.projection.size mustBe 3
      planet.projection(0) mustEqual planet
      planet.projection(1) mustEqual planet.copy(size = 105, fleets=List(Fleet(Him, 33, -1, 1)))
      planet.projection(2) mustEqual endState
      planet.projection.worst mustEqual 2
      planet.projection.worstPlanet mustEqual endState
      planet.projection.last mustEqual 2
      planet.projection.lastPlanet mustEqual endState
      planet.projection.transitionFromNeutralToEnemy must beNone
    }
  }

  "my planet that will have more regen than enemy fleet size" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 15, List(Fleet(Him, 20, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 110, owner = Me, fleets = Nil)
      planet.projection.size mustBe 3
      planet.projection(0) mustEqual planet
      planet.projection(1) mustEqual planet.copy(size = 115, fleets=List(Fleet(Him, 20, -1, 1)))
      planet.projection(2) mustEqual endState
      planet.projection.worst mustEqual 0
      planet.projection.worstPlanet mustEqual planet
      planet.projection.last mustEqual 2
      planet.projection.lastPlanet mustEqual endState
      planet.projection.transitionFromNeutralToEnemy must beNone
    }
  }

  "my planet that will receive re-inforcements" should {
    val planet = Planet(1, 1.0, 1.0, Me, 100, 5, List(Fleet(Me, 125, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 235, owner = Me, fleets = Nil)
      planet.projection.size mustBe 3
      planet.projection(0) mustEqual planet
      planet.projection(1) mustEqual planet.copy(size = 105, fleets=List(Fleet(Me, 125, -1, 1)))
      planet.projection(2) mustEqual endState
      planet.projection.worst mustEqual 0
      planet.projection.worstPlanet mustEqual planet
      planet.projection.last mustEqual 2
      planet.projection.lastPlanet mustEqual endState
      planet.projection.transitionFromNeutralToEnemy must beNone
    }
  }

  "neutral planet that exepects no fleets" should {
    val planet = Planet(1, 1.0, 1.0, Nobody, 50, 5, Nil)

    "have the current state for all three phases of the projection" in {
      planet.projection.size mustBe 1
      planet.projection(0) mustEqual planet
      planet.projection.worst mustEqual 0
      planet.projection.worstPlanet mustEqual planet
      planet.projection.last mustEqual 0
      planet.projection.lastPlanet mustEqual planet
      planet.projection.transitionFromNeutralToEnemy must beNone
    }
  }

  "neutral planet that will not be taken by my fleets" should {
    val planet = Planet(1, 1.0, 1.0, Nobody, 50, 5, List(Fleet(Me, 35, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 15, owner = Nobody, fleets = Nil)
      planet.projection.size mustBe 3
      planet.projection(0) mustEqual planet
      planet.projection(1) mustEqual planet.copy(fleets=List(Fleet(Me, 35, -1, 1)))
      planet.projection(2) mustEqual endState
      planet.projection.worst mustEqual 0
      planet.projection.worstPlanet mustEqual planet
      planet.projection.last mustEqual 2
      planet.projection.lastPlanet mustEqual endState
      planet.projection.transitionFromNeutralToEnemy must beNone
    }
  }

  "neutral planet that will be taken by my fleets" should {
    val planet = Planet(1, 1.0, 1.0, Nobody, 50, 5, List(Fleet(Me, 85, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 35, owner = Me, fleets = Nil)
      planet.projection.size mustBe 3
      planet.projection(0) mustEqual planet
      planet.projection(1) mustEqual planet.copy(fleets=List(Fleet(Me, 85, -1, 1)))
      planet.projection(2) mustEqual endState
      planet.projection.worst mustEqual 0
      planet.projection.worstPlanet mustEqual planet
      planet.projection.last mustEqual 2
      planet.projection.lastPlanet mustEqual endState
      planet.projection.transitionFromNeutralToEnemy must beNone
    }
  }

  "neutral planet that will not be taken by enemy fleets" should {
    val planet = Planet(1, 1.0, 1.0, Nobody, 50, 5, List(Fleet(Him, 35, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 15, owner = Nobody, fleets = Nil)
      planet.projection.size mustBe 3
      planet.projection(0) mustEqual planet
      planet.projection(1) mustEqual planet.copy(fleets=List(Fleet(Him, 35, -1, 1)))
      planet.projection(2) mustEqual endState
      planet.projection.worst mustEqual 0
      planet.projection.worstPlanet mustEqual planet
      planet.projection.last mustEqual 2
      planet.projection.lastPlanet mustEqual endState
      planet.projection.transitionFromNeutralToEnemy must beNone
    }
  }

  "neutral planet that will be taken by enemy fleets" should {
    val planet = Planet(1, 1.0, 1.0, Nobody, 50, 5, List(Fleet(Him, 85, -1, 2)))

    "have the initial state as the worst state" in {
      val endState = planet.copy(size = 35, owner = Him, fleets = Nil)
      planet.projection.size mustBe 3
      planet.projection(0) mustEqual planet
      planet.projection(1) mustEqual planet.copy(fleets=List(Fleet(Him, 85, -1, 1)))
      planet.projection(2) mustEqual endState
      planet.projection.worst mustEqual 0
      planet.projection.worstPlanet mustEqual planet
      planet.projection.last mustEqual 2
      planet.projection.lastPlanet mustEqual endState
      planet.projection.transitionFromNeutralToEnemy must beSome(2)
    }
  }

  "neutral planet that will be taken by massive enemy fleets" should {
    val planet = Planet(1, 1.0, 1.0, Nobody, 50, 5, List(Fleet(Him, 135, -1, 2)))

    "have the final state as the worst state" in {
      val endState = planet.copy(size = 85, owner = Him, fleets = Nil)
      planet.projection.size mustBe 3
      planet.projection(0) mustEqual planet
      planet.projection(1) mustEqual planet.copy(fleets=List(Fleet(Him, 135, -1, 1)))
      planet.projection(2) mustEqual endState
      planet.projection.worst mustEqual 2
      planet.projection.worstPlanet mustEqual endState
      planet.projection.last mustEqual 2
      planet.projection.lastPlanet mustEqual endState
      planet.projection.transitionFromNeutralToEnemy must beSome(2)
    }
  }


}