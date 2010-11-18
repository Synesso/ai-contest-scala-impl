package jem

import scala.math.{ceil,sqrt,max,abs}

case class Planet(index: Int, x: Double, y: Double, owner: Owner, size: Int, regen: Int, fleets: Seq[Fleet]) {

  def distanceTo(other: Planet): Int = distanceTo((other.x, other.y))

  def distanceTo(xy: (Double, Double)) = {
    val dx = x - xy._1
    val dy = y - xy._2
    ceil(sqrt(dx * dx + dy * dy)).asInstanceOf[Int]
  }

  def hasSurplus = Me.equals(owner) && size > 2
  def hasDeficit = !Me.equals(owner) && regen > 0
  def afterSending(amount: Int) = Planet(index, x, y, owner, size - amount, regen, fleets)

  lazy val projection = Projection.of(this)

}