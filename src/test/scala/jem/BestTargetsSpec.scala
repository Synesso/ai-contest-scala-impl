package jem

import org.specs.Specification

class BestTargetsSpec extends Specification {
  val function = (new PassTheParcelBot).bestTargets _

  "A list of targets that falls within the surplus amount" should {
    "all be serviced" in {
      val targets = Planet(1, 0, 0, Nobody, 20, 2, Nil).projection ::
        Planet(2, 0, 0, Nobody, 20, 2, Nil).projection :: Nil
      val surplus = 42
      function(targets, surplus) must_== targets
    }
  }

  "A list of targets where none can be serviced by the surplus amount" should {
    "have none serviced" in {
      val targets = Planet(1, 0, 0, Nobody, 20, 2, Nil).projection ::
        Planet(2, 0, 0, Nobody, 20, 2, Nil).projection :: Nil
      val surplus = 18
      function(targets, surplus) must beEmpty
    }
  }

  "A list of targets where some can be serviced by the surplus amount" should {
    "have those serviced (with the list order maintained)" in {
      val targets = Planet(1, 0, 0, Nobody, 20, 2, Nil).projection ::
        Planet(2, 0, 0, Nobody, 40, 2, Nil).projection ::
        Planet(2, 0, 0, Nobody, 10, 2, Nil).projection :: Nil
      val surplus = 33
      function(targets, surplus) must_== targets(0) :: targets(2) :: Nil 
    }
  }

  "A list of targets where some can be serviced exactly by the surplus amount" should {
    "have those serviced (with the list order maintained)" in {
      val targets = Planet(1, 0, 0, Nobody, 20, 2, Nil).projection ::
        Planet(2, 0, 0, Nobody, 40, 2, Nil).projection ::
        Planet(2, 0, 0, Nobody, 10, 2, Nil).projection :: Nil
      val surplus = 62
      function(targets, surplus) must_== targets(0) :: targets(1) :: Nil 
    }
  }
}