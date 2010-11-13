package jem

import collection.Seq
import scala.math.min

class PassTheParcelBot extends Bot {
  override def respondTo(turnState: Seq[Planet]): Set[Order] = {
    val projections = turnState.map(_.projection)
    val (senders, receivers) = projections.filter(_.regen > 0).partition(_.surplus > 0)

    val cumulativeCoordinates = senders.foldLeft((0.0, 0.0)) ((acc, next) => (acc._1 + next.x, acc._2 + next.y))
    val centreOfSenders = (cumulativeCoordinates._1 / senders.size, cumulativeCoordinates._2 / senders.size)
    val scorer = new CandidateScore(centreOfSenders)

    val orders = linkSendersToTargets(senders, receivers, scorer)
    val ordersViaFriends = viaClosestRoute(orders, senders)
    val nettedOrders = nett(orders)

    ordersViaFriends
  }

  def linkSendersToTargets(senders: Seq[Projection], receivers: Seq[Projection], scorer: CandidateScore) = {
    val receiversNotBeingTaken = receivers.filterNot(_.last._1.owner.equals(Me))
    val targets = receiversNotBeingTaken.toList.sortWith((r1, r2) => scorer.forTarget(r1) < scorer.forTarget(r2))
    val (result, _) = targets.foldLeft(Set.empty[Order], senders) {(acc, target) =>
      val (orders, remainingSenders) = acc
      val (newOrders, depletedSenders) = ordersForTarget(target, remainingSenders)
      (orders ++ newOrders, depletedSenders)
    }
    result
  }

  def ordersForTarget(target: Projection, senders: Seq[Projection]): (Set[Order], Seq[Projection]) = {
    val result = senders.foldLeft((Set.empty[Order], List.empty[Projection], target.surplus)) {(acc, sender) =>
      val (ordersSoFar, unusedSenders, deficit) = acc
      if (deficit > 0) (ordersSoFar, sender :: unusedSenders, deficit)
      else if (sender.surplus > 0) {
        val amount: Int = if (deficit == 0) 1 else min(deficit * -1, sender.surplus)
        val order: Order = Order(sender.current, target.current, amount)
        val newOrders: Set[Order] = ordersSoFar + order
        if (amount == sender.surplus) (newOrders, unusedSenders, deficit + amount)
        else (newOrders, sender.afterSending(amount) :: unusedSenders, deficit + amount)
      } else acc
    }
    (result._1, result._2)
  }

  def viaClosestRoute(orders: Set[Order], friendly: Seq[Projection]): Set[Order] = orders.map{o =>
      val notSource = friendly.filterNot(_.current.equals(o.from))
      val closerThanDestination = notSource.filter(_.distanceTo(o.to) < o.from.distanceTo(o.to))
      val notOffCourse = closerThanDestination.filter(p => p.distanceTo(o.from) + p.distanceTo(o.to) < o.from.distanceTo(o.to) * 1.2)
      val closest = notOffCourse.sortWith(_.distanceTo(o.from) < _.distanceTo(o.from))
      closest.headOption.map(p => o.copy(to = p.current)).getOrElse(o)
    }

  def nett(orders:Set[Order]) = {
    val netted = orders.foldLeft(Map.empty[(Planet, Planet), Int].withDefaultValue(0)) {(map, order) =>
      val (from, to) = (order.from, order.to)
      if (from.index < to.index) map.updated((from, to), map(from, to) + order.quantity)
      else map.updated((to, from), map(to, from) + (order.quantity * -1))
    }
    netted.foldLeft(Set.empty[Order]){(nettedOrders, entry) =>
      val (planets, amount) = entry
      if (amount < 0) nettedOrders + Order(planets._2, planets._1, amount * -1)
      else if (amount > 0) nettedOrders + Order(planets._1, planets._2, amount)
      else nettedOrders
    }
  }
/*
  def nett(orders:Set[Order]) = {
    orders.foldLeft(Set.empty[Order]) {(newOrders, order) =>
      val (samePath, rest) = orders.partition(o => o.from == order.from && o.to == order.to)
      val nettedSamePath = samePath.reduceLeft((o1, o2) => o1.copy(quantity = o1.quantity + o2.quantity))
      rest + nettedSamePath
    }
  }
*/

}