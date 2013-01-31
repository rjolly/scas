package scas.ordering

class DegreeReverseLexicographic[@specialized(Int, Long) N](implicit val nm: scala.math.Ordering[N]) extends Ordering[N] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  def compare(x: Array[N], y: Array[N]): Int = {
    val n = x.length - 1
    if (x(n) < y(n)) -1
    else if (x(n) > y(n)) 1
    else {
      var i = 0
      while (i < n - 1 && (x(i) equiv y(i))) i += 1
      if (x(i) > y(i)) -1
      else if (x(i) < y(i)) 1
      else 0
    }
  }
}
