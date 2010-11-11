package jem

import scala.math.{min, max}

case class Projection(current: Planet, worst: (Planet, Int), last: (Planet, Int)) {
  val regen = current.regen
  val surplus = (current.owner, current.size, worst._1.owner, worst._1.size) match {
    case (Me, x, Me, y) => min(x, y) - 1
    case (Me, x, _, y) => y * -1 - 1
    case (_, x, Me, y) => x * -1 - 1
    case (_, x, _, y) => max(x, y) * -1 - 1
  }
  def distanceTo(xy: (Double, Double)) = current.distanceTo(xy)
  val x = current.x
  val y = current.y
}