package scas.power.growable

import scas.math.Numeric

trait ArrayPowerProduct[N : Numeric as numeric] extends scas.power.ArrayPowerProduct[N] with PowerProduct[Array[N]] {
  extension (x: Array[N]) override def get(i: Int) = {
    assert (i < x.length)
    if i < x.length then x(i) else numeric.zero
  }
}
