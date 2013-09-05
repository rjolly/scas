package scas.power.offset

import scala.reflect.ClassTag
import scas.Variable
import scas.math.{Ordering, Numeric}
import Ordering.Implicits.infixOrderingOps
import PowerProduct.lexicographic

class Lexicographic[@specialized(Byte, Short, Int, Long) N](val variables: Array[Variable])(implicit val nm: Numeric[N], val m: ClassTag[N], val cm: ClassTag[Array[N]]) extends PowerProductWithDegree[N] {
  import variables.{length => n}
  def self(variables: Array[Variable]) = lexicographic[N](variables: _*)
  def compare(x: Array[N], k: Int, y: Array[N], l: Int): Int = {
    var i = n + k
    var j = n + l
    while (i > k) {
      i -= 1
      j -= 1
      if (x(i) < y(j)) return -1
      if (x(i) > y(j)) return 1
    }
    return 0
  }
}
