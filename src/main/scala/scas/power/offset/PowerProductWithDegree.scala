package scas.power.offset

import scas.math.Numeric
import Numeric.Implicits.infixNumericOps

trait PowerProductWithDegree[@specialized(Byte, Short, Int, Long) N] extends scas.power.PowerProductWithDegree[N] with PowerProduct[N] {
  import variables.length
  def times(x: Array[N], k: Int, y: Array[N], l: Int) = {
    val r = one
    var i = 0
    while (i <= length) {
      r(i) = x(i + k) + y(i + l)
      i += 1
    }
    r
  }
}
