package scas.polynomial.ordering

class DegreeLexicographic[@specialized(Int, Long) N](implicit val nm: scala.math.Ordering[N]) extends Ordering[N] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  def compare(x: Array[N], y: Array[N]): Int = {
    val n = x.length
    for (i <- n - 1 to 0 by -1) {
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    0
  }
}

object DegreeLexicographic {
  def apply[@specialized(Int, Long) N](implicit nm: scala.math.Ordering[N]) = new DegreeLexicographic[N]
}
