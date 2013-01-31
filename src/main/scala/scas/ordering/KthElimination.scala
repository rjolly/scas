package scas.ordering

class KthElimination[@specialized(Int, Long) N](val k: Int)(implicit val nm: scala.math.Ordering[N]) extends Ordering[N] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  override def compare(x: Array[N], y: Array[N]): Int = {
    val n = x.length - 1
    var i = n - 1
    while (i > n - k && (x(i) equiv y(i))) i -= 1
    if (x(i) < y(i)) -1
    else if (x(i) > y(i)) 1
    else Ordering.degreeReverseLexicographic[N].compare(x, y)
  }
}
