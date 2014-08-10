package scas.power

import scala.reflect.ClassTag
import scas.Variable
import scas.math.{Ordering, Numeric}
import Ordering.Implicits.infixOrderingOps
import PowerProduct.degreeReverseLexicographic

class KthElimination[@specialized(Byte, Short, Int, Long) N](val variables: Array[Variable], val k: Int)(implicit val nm: Numeric[N], val m: ClassTag[N], val cm: ClassTag[Array[N]]) extends PowerProductWithDegree[N] {
  val self = this
  import variables.{length => n}
  val ordering = degreeReverseLexicographic[N](variables: _*)
  def compare(x: Array[N], y: Array[N]): Int = {
    var i = n - 1
    while (i > n - k && (x(i) equiv y(i))) i -= 1
    if (x(i) < y(i)) -1
    else if (x(i) > y(i)) 1
    else ordering.compare(x, y)
  }
}
