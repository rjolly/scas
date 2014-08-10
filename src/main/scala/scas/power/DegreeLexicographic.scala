package scas.power

import scala.reflect.ClassTag
import scas.Variable
import scas.math.{Ordering, Numeric}
import Ordering.Implicits.infixOrderingOps
import PowerProduct.lexicographic

class DegreeLexicographic[@specialized(Byte, Short, Int, Long) N](val variables: Array[Variable])(implicit val nm: Numeric[N], val m: ClassTag[N], val cm: ClassTag[Array[N]]) extends PowerProductWithDegree[N] {
  val self = this
  import variables.{length => n}
  val ordering = lexicographic[N](variables: _*)
  def compare(x: Array[N], y: Array[N]): Int = {
    if (x(n) < y(n)) -1
    else if (x(n) > y(n)) 1
    else ordering.compare(x, y)
  }
}
