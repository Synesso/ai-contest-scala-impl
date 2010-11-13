package jem

import org.specs.Specification
import collection.immutable.Set

class NettOrdersSpec extends Specification {

  val nett = (new PassTheParcelBot).nett _

  val planet01 = Planet(1, 0.0, 0.0, Me, 50, 5, Nil)
  val planet02 = Planet(2, 5.0, 5.0, Me, 50, 5, Nil)
  val planet03 = Planet(3, 8.0, 8.0, Me, 50, 5, Nil)

  "No orders" should {
    "nett to no orders" in {
      nett(Set.empty[Order]) must beEmpty
    }
  }

  "One order" should {
    "remain unchanged" in {
      val orders = Set(Order(planet01, planet02, 10))
      nett(orders) must_== orders
    }
  }

  "Two orders to different places" should {
    "remain unchanged" in {
      val orders = Set(Order(planet01, planet02, 10), Order(planet01, planet03, 10))
      nett(orders) must_== orders
    }
  }

  "Two orders from different places" should {
    "remain unchanged" in {
      val orders = Set(Order(planet01, planet02, 10), Order(planet03, planet02, 10))
      nett(orders) must_== orders
    }
  }

  "Two orders between the same two places" should {
    "be netted into one" in {
      val orders = Set(Order(planet01, planet02, 10), Order(planet02, planet01, 15))
      nett(orders) must_== Set(Order(planet02, planet01, 5))
    }
  }

  "Two orders between the same two places which cancel each other out" should {
    "be netted into nothing" in {
      val orders = Set(Order(planet01, planet02, 10), Order(planet02, planet01, 10))
      nett(orders) must beEmpty
    }
  }

}