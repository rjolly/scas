package scas.ordering

class Lexicographic[@specialized(Int, Long) N](implicit val nm: scala.math.Ordering[N]) extends Ordering[N] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  def compare(x: Array[N], y: Array[N]): Int = {
    val n = x.length - 1
    var i = n
    while (i > 0) {
      i -= 1
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    return 0
  }
}
