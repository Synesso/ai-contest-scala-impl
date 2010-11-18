package jem

import org.specs.Specification

class OrdersForTargetSpec extends Specification {
  val ordersForTarget = (new PassTheParcelBot).ordersForTarget _
  val neutralPlanet = Planet(0, 0, 0, Nobody, 50, 5, Nil)
  val myPlanet = Planet(0, 0, 0, Me, 30, 5, Nil)
  val target = neutralPlanet.projection

  "No senders" should {
    "result in no orders being issued" in {
      val (orders, remainingSenders) = ordersForTarget(target, Nil)
      orders must beEmpty
      remainingSenders must beEmpty
    }
  }

  "A sender with less than the target needs" should {
    "result in one order being issued and no senders remaining" in {
      val (orders, remainingSenders) = ordersForTarget(target, List(myPlanet.projection))
      orders must haveTheSameElementsAs(Set(Order(myPlanet, target.current, myPlanet.size - 1)))
      remainingSenders must beEmpty
    }
  }

  "A sender with more than the target needs" should {
    "result in one order being issued and one sender remaining" in {
      val sender = myPlanet.copy(size = 80)
      val (orders, remainingSenders) = ordersForTarget(target, List(sender.projection))
      orders must haveTheSameElementsAs(Set(Order(sender, target.current, 51)))
      remainingSenders must haveTheSameElementsAs(Set(sender.copy(size = 29).projection))
    }
  }

  "Several senders with less than the target needs" should {
    "result in several orders and no senders remaining" in {
      val target = Planet(0, 0, 0, Nobody, 100, 5, Nil).projection
      val sender01 = Planet(1, 5, 5, Me, 30, 5, Nil).projection
      val sender02 = Planet(2, 8, 8, Me, 30, 5, Nil).projection

      val (orders, remainingSenders) = ordersForTarget(target, List(sender01, sender02))
      orders must haveTheSameElementsAs(Set(
        Order(sender01.current, target.current, 29),
        Order(sender02.current, target.current, 29)))
      remainingSenders must beEmpty
    }
  }

  "Several senders with exactly what the target needs" should {
    "result in several orders and no senders remaining" in {
      val target = Planet(0, 0, 0, Nobody, 59, 5, Nil).projection
      val sender01 = Planet(1, 5, 5, Me, 31, 5, Nil).projection
      val sender02 = Planet(2, 8, 8, Me, 31, 5, Nil).projection

      val (orders, remainingSenders) = ordersForTarget(target, List(sender01, sender02))
      orders must haveTheSameElementsAs(Set(
        Order(sender01.current, target.current, 30),
        Order(sender02.current, target.current, 30)))
      remainingSenders must beEmpty
    }
  }

  "Several senders with more than what the target needs" should {
    "result in several orders and senders remaining" in {
      val target = Planet(0, 0, 0, Nobody, 49, 5, Nil).projection
      val furtherestSender = Planet(2, 8, 8, Me, 31, 5, Nil).projection
      val closestSender = Planet(1, 5, 5, Me, 31, 5, Nil).projection

      val (orders, remainingSenders) = ordersForTarget(target, List(furtherestSender, closestSender))
      orders must haveTheSameElementsAs(Set(
        Order(closestSender.current, target.current, 30),
        Order(furtherestSender.current, target.current, 20)))
      remainingSenders must haveTheSameElementsAs(Set(furtherestSender.current.copy(size = 11).projection))
    }
  }

  "A sender with nothing to give" should {
    "not issue any order" in {
      val target = Planet(0, 0, 0, Nobody, 49, 5, Nil).projection
      val sender = Planet(1, 1, 1, Me, 1, 5, Nil).projection
      val (orders, remainingSenders) = ordersForTarget(target, List(sender))
      orders must beEmpty
      remainingSenders must beEmpty
    }
  }

  "A target with a surplus of -1" should {
    "receive an order of size 1 when available" in {
      val target = Planet(0, 0, 0, Me, 0, 5, Nil).projection
      val sender = Planet(1, 1, 1, Me, 50, 5, Nil).projection
      val (orders, remainingSenders) = ordersForTarget(target, List(sender))
      orders must haveTheSameElementsAs(Set(Order(sender.current, target.current, 1)))
      remainingSenders must haveTheSameElementsAs(Set(sender.current.copy(size = 49).projection))
    }
  }

  "A target with a surplus of zero" should {
    "Not receive any order" in {
      val target = Planet(0, 0, 0, Me, 1, 5, Nil).projection
      val sender = Planet(1, 1, 1, Me, 50, 5, Nil).projection
      val (orders, remainingSenders) = ordersForTarget(target, List(sender))
      orders must beEmpty
      remainingSenders must haveTheSameElementsAs(Set(sender))
    }
  }
}
