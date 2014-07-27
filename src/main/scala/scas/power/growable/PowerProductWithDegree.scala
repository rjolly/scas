package scas.power.growable

import scas.math.Numeric
import Numeric.Implicits.infixNumericOps

trait PowerProductWithDegree[@specialized(Byte, Short, Int, Long) N] extends scas.power.PowerProductWithDegree[N] {
  import nm.fromInt
  import variables.length
  override def times(x: Array[N], y: Array[N]) = {
    val r = one
    var i = 0
    while (i < length) {
      r(i) = get(x, i) + get(y, i)
      i += 1
    }
    r(length) = x(x.length - 1) + y(y.length - 1)
    r
  }

  override def get(x: Array[N], i: Int) = if (i < x.length - 1) x(i) else fromInt(0)
}
