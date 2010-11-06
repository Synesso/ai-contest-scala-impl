package jem

import collection.Seq

class PassTheParcelBot extends Bot {
  override def respondTo(turnState: Seq[Planet]): Set[Order] = {
    val withIndex: Seq[(Planet, Int)] = turnState.zipWithIndex
    val (ownedByMe, notOwnedByMe) = withIndex.partition(pi => pi._1.owner.equals(Me))
    val cumulativeCoordinates = ownedByMe.foldLeft((0.0, 0.0)) ((acc, next) => (acc._1 + next._1.x, acc._2 + next._1.y))
    val centreOfMine = (cumulativeCoordinates._1 / ownedByMe.size, cumulativeCoordinates._2 / ownedByMe.size)
    val notOwnedByMeWithRegen = notOwnedByMe.filter(_._1.regen > 0)
    val target = if (notOwnedByMeWithRegen.isEmpty) None else Some (notOwnedByMeWithRegen.reduceLeft{(pi1, pi2) =>
      if (pi1._1.distanceTo(centreOfMine) > pi2._1.distanceTo(centreOfMine)) pi2 else pi1
    })
    val canGive = ownedByMe.filter(_._1.size > 1)
    val orders = canGive.flatMap(pi => target.map(t => new Order(pi._1, t._1, pi._1.size - 1)))
    val ordersViaFriends = orders.map{o =>
      val notFrom = withIndex.filterNot(_._1 == o.from)
      val closerThanDestination = notFrom.filter(_._1.distanceTo(o.to) < o.from.distanceTo(o.to))
      val notOffCourse = closerThanDestination.filter(pi => pi._1.distanceTo(o.from) + pi._1.distanceTo(o.to) < o.from.distanceTo(o.to) * 1.2)
      val closest = notOffCourse.sortWith((pi1, pi2) => pi1._1.distanceTo(o.from) < pi2._1.distanceTo(o.from))
      o.copy(to = closest.head._1)
    }
    ordersViaFriends.toSet
  }
}