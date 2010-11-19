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
    val receiversNotBeingTaken = receivers.filterNot(_.lastPlanet.owner.equals(Me))
    val surplus = senders.foldLeft(0)((acc, next) => acc + next.surplus)
    val allTargets = receiversNotBeingTaken.toList.sortWith((r1, r2) => scorer.forTarget(r1) < scorer.forTarget(r2))
    val targets = bestTargets(allTargets, surplus)
    val (result, _) = targets.foldLeft(Set.empty[Order], senders) {(acc, target) =>
      val (orders, remainingSenders) = acc
      val (newOrders, depletedSenders) = ordersForTarget(target, remainingSenders)
      (orders ++ newOrders, depletedSenders)
    }
    result
  }

  def bestTargets(sortedTargets: List[Projection], surplus: Int) = {
    val (selected, _, _) = sortedTargets.foldLeft(List.empty[Projection], surplus, 0) {(acc, next) =>
      val (selectedSoFar, remainingSurplus, skipCount) = acc
      if (next.surplus * -1 <= remainingSurplus && skipCount < 3) (next :: selectedSoFar, remainingSurplus + next.surplus, skipCount)
      else (selectedSoFar, remainingSurplus, skipCount + 1)
    }
    selected.reverse
  }

  def ordersForTarget(target: Projection, senders: Seq[Projection]): (Set[Order], Seq[Projection]) = {
    val bestDistance = target.transitionFromNeutralToEnemy.getOrElse(0)
    val (sendersNotTooClose, sendersTooClose) = senders.partition(_.distanceTo(target) >= bestDistance)
    val sortedSenders = sendersNotTooClose.toList.sortWith((p1, p2) => p1.distanceTo(target) < p2.distanceTo(target))
    val result = sortedSenders.foldLeft((Set.empty[Order], List.empty[Projection], target.surplus)) {(acc, sender) =>
      val (ordersSoFar, unusedSenders, deficit) = acc
      if (deficit > 0) (ordersSoFar, sender :: unusedSenders, deficit)
      else if (sender.surplus > 0) {
        val amount = min(deficit * -1, sender.surplus)
        val newOrders = if (amount > 0) ordersSoFar + Order(sender(0), target(0), amount) else ordersSoFar
        if (amount == sender.surplus) (newOrders, unusedSenders, deficit + amount)
        else (newOrders, sender.afterSending(amount) :: unusedSenders, deficit + amount)
      } else acc
    }
    (result._1, result._2 ++ sendersTooClose)
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
}