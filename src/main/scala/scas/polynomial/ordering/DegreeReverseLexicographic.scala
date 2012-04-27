package scas.polynomial.ordering

class DegreeReverseLexicographic[@specialized(Int, Long) N](implicit val nm: scala.math.Ordering[N]) extends Ordering[N] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  def compare(x: Array[N], y: Array[N]): Int = {
    val n = x.length - 1
    if (x(n) < y(n)) return -1
    else if (x(n) > y(n)) return 1
    for (i <- 0 until n) {
      if (x(i) > y(i)) return -1
      else if (x(i) < y(i)) return 1
    }
    0
  }
}

object DegreeReverseLexicographic {
  def apply[@specialized(Int, Long) N](implicit nm: scala.math.Ordering[N]) = new DegreeReverseLexicographic[N]
}
