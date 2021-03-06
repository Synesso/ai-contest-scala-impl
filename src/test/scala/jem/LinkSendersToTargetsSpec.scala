package jem

import org.specs.Specification

class LinkSendersToTargetsSpec extends Specification {
  val scorer = new CandidateScore(0.0, 0.0)
  val function = (new PassTheParcelBot).linkSendersToTargets(_: Seq[Projection], _: Seq[Projection], scorer)

  "a map with one sender and one target" should {
    "order a fleet from sender to target" in {
      val senders = List(Planet(1, 0.0, 0.0, Me, 51, 5, Nil).projection)
      val targets = List(Planet(2, 1.0, 1.0, Nobody, 49, 5, Nil).projection)
      val orders = function(senders, targets)
      orders must haveTheSameElementsAs(Set{
        Order(senders(0).current, targets(0).current, 50)
      })
    }
  }

  "a map with one undersupplied sender and two targets" should {
    "order a fleet from sender to best target" in {
      val senders = List(Planet(1, 0.0, 0.0, Me, 50, 5, Nil).projection)
      val target01 = Planet(2, 5.0, 5.0, Nobody, 48, 5, Nil).projection
      val target02 = Planet(3, 3.0, 3.0, Nobody, 48, 5, Nil).projection
      val targets = List(target01, target02)
      val orders = function(senders, targets)
      orders must haveTheSameElementsAs(Set{
        Order(senders(0).current, target02.current, 49)
      })
    }
  }

  "a map with a target that is going to be mine" should {
    "issue no orders" in {
      val senders = List(Planet(1, 0, 0, Me, 50, 5, Nil).projection)
      val targets = List(Planet(2, 3, 3, Nobody, 20, 5, List(Fleet(Me, 21, -1, 3))).projection)
      val orders = function(senders, targets)
      orders must beEmpty
    }
  }

  "a map with a one sender, one redundant surplus planet and one target" should {
    "issue fleets from the sender, being closest" in {
      val senders = List(Planet(1, 3, 3, Me, 50, 5, Nil).projection, Planet(2, 5, 5, Me, 50, 5, Nil).projection)
      val targets = List(Planet(3, 0, 0, Nobody, 30, 5, Nil).projection)
      val orders = function(senders, targets)
      orders must haveTheSameElementsAs(Set{
        Order(senders(0).current, targets(0).current, 31)
      })
    }
  }

  "a map with mirrored targets and senders" should {
    "issue fleets to each target from its closest sender" in {
      val senders = Planet(1, 2, 2, Me, 90, 5, Nil).projection :: Planet(2, 8, 8, Me, 90, 5, Nil).projection :: Nil
      val targets = Planet(3, 3, 3, Nobody, 20, 5, Nil).projection :: Planet(4, 7, 7, Nobody, 20, 5, Nil).projection :: Nil
      val orders = function(senders, targets)
      orders must haveTheSameElementsAs(Set(
        Order(senders(0).current, targets(0).current, 21),
        Order(senders(1).current, targets(1).current, 21)
      ))
    }
  }

  "a map with a sender that cannot take a target" should {
    "issue no orders" in {
      val senders = Planet(1, 0, 0, Me, 25, 5, Nil).projection :: Nil
      val targets = Planet(2, 0, 0, Nobody, 45, 5, Nil).projection :: Nil
      val orders = function(senders, targets)
      orders must beEmpty
    }
  }
}