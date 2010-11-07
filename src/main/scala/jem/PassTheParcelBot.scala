package jem

import collection.Seq

class PassTheParcelBot extends Bot {
  override def respondTo(turnState: Seq[Planet]): Set[Order] = {
    val leastFavourableFutureState = turnState.map(_.leastFavourableProjection)
    val nowAndLater: Seq[(Planet, Planet)] = turnState.zip(leastFavourableFutureState)
    val senders = nowAndLater.filter(nl => nl._1.hasSurplus && nl._2.hasSurplus)
    val receivers = nowAndLater.filter(nl => nl._2.hasDeficit)

    val cumulativeCoordinates = senders.foldLeft((0.0, 0.0)) ((acc, next) => (acc._1 + next._1.x, acc._2 + next._1.y))
    val centreOfSenders = (cumulativeCoordinates._1 / senders.size, cumulativeCoordinates._2 / senders.size)

    val target = if (receivers.isEmpty) None else Some (receivers.reduceLeft{(nl1, nl2) =>
      if (nl1._1.distanceTo(centreOfSenders) > nl2._1.distanceTo(centreOfSenders)) nl2 else nl1
    })
    val orders = senders.flatMap(nl => target.map(t => new Order(nl._1, t._1, nl._1.size - 1)))
    val ordersViaFriends = orders.map{o =>
      val notFrom = senders.filterNot(_._1 == o.from)
      val closerThanDestination = notFrom.filter(_._1.distanceTo(o.to) < o.from.distanceTo(o.to))
      val notOffCourse = closerThanDestination.filter(nl => nl._1.distanceTo(o.from) + nl._1.distanceTo(o.to) < o.from.distanceTo(o.to) * 1.2)
      val closest = notOffCourse.sortWith((nl1, nl2) => nl1._1.distanceTo(o.from) < nl2._1.distanceTo(o.from))
      closest.headOption.map(nl => o.copy(to = nl._1)).getOrElse(o)
    }
    ordersViaFriends.toSet
  }
}