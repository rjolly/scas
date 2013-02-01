package scas.ordering

class DegreeReverseLexicographic[@specialized(Int, Long) N](implicit val nm: scala.math.Ordering[N]) extends Ordering[N] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  def compare(x: Array[N], y: Array[N]): Int = {
    val n = x.length - 1
    if (x(n) < y(n)) -1
    else if (x(n) > y(n)) 1
    else {
      var i = 0
      while (i < n) {
        if (x(i) > y(i)) return -1
        else if (x(i) < y(i)) return 1
        i += 1
      }
      return 0
    }
  }
}
