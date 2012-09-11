package scas.polynomial.ordering

class DegreeLexicographic[@specialized(Int, Long) N](implicit val nm: scala.math.Ordering[N]) extends Ordering[N] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  def compare(x: Array[N], y: Array[N]): Int = {
    val n = x.length - 1
    if (x(n) < y(n)) -1
    else if (x(n) > y(n)) 1
    else Ordering.lexicographic[N].compare(x, y)
  }
}
