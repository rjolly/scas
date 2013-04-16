package scas.power

import scala.reflect.ClassTag
import scas.Variable
import scas.math.{Ordering, Numeric}
import Ordering.Implicits.infixOrderingOps
import PowerProduct.{degreeLexicographic, lexicographic}

class DegreeLexicographic[@specialized(Byte, Short, Int, Long) N](val variables: Array[Variable])(implicit val nm: Numeric[N], val m: ClassTag[N], val cm: ClassTag[Array[N]]) extends PowerProduct[N] {
  val ordering = lexicographic[N](variables: _*)
  def self(variables: Array[Variable]) = degreeLexicographic[N](variables: _*)
  def compare(x: Array[N], y: Array[N]): Int = {
    val n = length
    if (x(n) < y(n)) -1
    else if (x(n) > y(n)) 1
    else ordering.compare(x, y)
  }
}
