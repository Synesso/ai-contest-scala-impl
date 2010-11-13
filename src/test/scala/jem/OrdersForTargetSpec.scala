package jem

import org.specs.Specification

class OrdersForTargetSpec extends Specification {
  val ordersForTarget = (new PassTheParcelBot).ordersForTarget _
  val neutralPlanet = Planet(0, 0, 0, Nobody, 50, 5, Nil)
  val myPlanet = Planet(0, 0, 0, Me, 30, 5, Nil)
  val target = Projection(neutralPlanet, (neutralPlanet, 0), (neutralPlanet, 0))
  val myPlanetAsProjection = Projection()

  "No senders" should {
    "result in no orders being issued" in {
      val (orders, remainingSenders) = ordersForTarget(target, Nil)
      orders must beEmpty
      remainingSenders must beEmpty
    }
  }

  "A sender with less than the target needs" should {
    "result in one order being issued and no senders remaining" in {
      val (orders, remainingSenders) = ordersForTarget(target, List(projectionOf(myPlanet)))
      orders must haveTheSameElementsAs(Set(Order(myPlanet, target.current, myPlanet.size - 1)))
      remainingSenders must beEmpty
    }
  }

  "A sender with more than the target needs" should {
    "result in one order being issued and one sender remaining" in {
      val sender = myPlanet.copy(size = 80)
      val (orders, remainingSenders) = ordersForTarget(target, List(projectionOf(sender)))
      orders must haveTheSameElementsAs(Set(Order(sender, target.current, 51)))
      remainingSenders must haveTheSameElementsAs(Set(projectionOf(sender.copy(size = 29))))
    }
  }

  "Several senders with less than the target needs" should {
    "result in several orders and no senders remaining" in {
      val target = projectionOf(Planet(0, 0, 0, Nobody, 100, 5, Nil))
      val sender01 = projectionOf(Planet(1, 5, 5, Me, 30, 5, Nil))
      val sender02 = projectionOf(Planet(2, 8, 8, Me, 30, 5, Nil))

      val (orders, remainingSenders) = ordersForTarget(target, List(sender01, sender02))
      orders must haveTheSameElementsAs(Set(
        Order(sender01.current, target.current, 29),
        Order(sender02.current, target.current, 29)))
      remainingSenders must beEmpty
    }
  }

  "Several senders with exactly what the target needs" should {
    "result in several orders and no senders remaining" in {
      val target = projectionOf(Planet(0, 0, 0, Nobody, 59, 5, Nil))
      val sender01 = projectionOf(Planet(1, 5, 5, Me, 31, 5, Nil))
      val sender02 = projectionOf(Planet(2, 8, 8, Me, 31, 5, Nil))

      val (orders, remainingSenders) = ordersForTarget(target, List(sender01, sender02))
      orders must haveTheSameElementsAs(Set(
        Order(sender01.current, target.current, 30),
        Order(sender02.current, target.current, 30)))
      remainingSenders must beEmpty
    }
  }

  "Several senders with more than what the target needs" should {
    "result in several orders and senders remaining" in {
      val target = projectionOf(Planet(0, 0, 0, Nobody, 49, 5, Nil))
      val sender01 = projectionOf(Planet(1, 5, 5, Me, 31, 5, Nil))
      val sender02 = projectionOf(Planet(2, 8, 8, Me, 31, 5, Nil))

      val (orders, remainingSenders) = ordersForTarget(target, List(sender01, sender02))
      orders must haveTheSameElementsAs(Set(
        Order(sender01.current, target.current, 30),
        Order(sender02.current, target.current, 20)))
      remainingSenders must haveTheSameElementsAs(Set(projectionOf(sender02.current.copy(size = 11))))
    }
  }

  "A sender with nothing to give" should {
    "not issue any order" in {
      val target = projectionOf(Planet(0, 0, 0, Nobody, 49, 5, Nil))
      val sender = projectionOf(Planet(1, 1, 1, Me, 1, 5, Nil))
      val (orders, remainingSenders) = ordersForTarget(target, List(sender))
      orders must beEmpty
      remainingSenders must beEmpty
    }
  }

  "A target with a surplus of zero" should {
    "receive an order of size 1 when available" in {
      val target = projectionOf(Planet(0, 0, 0, Me, 1, 5, Nil))
      val sender = projectionOf(Planet(1, 1, 1, Me, 50, 5, Nil))
      val (orders, remainingSenders) = ordersForTarget(target, List(sender))
      orders must haveTheSameElementsAs(Set(Order(sender.current, target.current, 1)))
      remainingSenders must haveTheSameElementsAs(Set(projectionOf(sender.current.copy(size = 49))))
    }
  }

  private def projectionOf(planet: Planet) = Projection(planet, (planet, 0), (planet, 0))
}
