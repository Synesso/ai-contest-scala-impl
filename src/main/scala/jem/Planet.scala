package jem

case class Planet(index: Int, x: Double, y: Double, owner: Owner, size: Int, regen: Int, fleets: Seq[Fleet]) {
  def distanceTo(other: Planet): Int = distanceTo((other.x, other.y))

  def distanceTo(xy: (Double, Double)) = {
    import scala.math.{ceil,sqrt}
    val dx = x - xy._1
    val dy = y - xy._2
    ceil(sqrt(dx * dx + dy * dy)).asInstanceOf[Int]
  }
}