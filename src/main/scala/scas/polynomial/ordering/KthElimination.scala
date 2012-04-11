package scas.polynomial.ordering

class KthElimination[@specialized(Int, Long) N](val k: Int)(implicit val nm: scala.math.Ordering[N]) extends Ordering[N] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  val ordering = DegreeReverseLexicographic[N]
  override def compare(x: Array[N], y: Array[N]): Int = {
    for (i <- 0 until k) {
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    ordering.compare(x, y)
  }
}

object KthElimination {
  def apply[@specialized(Int, Long) N](k: Int)(implicit nm: scala.math.Ordering[N]) = new KthElimination[N](k)
}
