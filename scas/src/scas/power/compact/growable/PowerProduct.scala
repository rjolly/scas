package scas.power.compact.growable

import scas.variable.Variable

abstract class PowerProduct(val shift: Int)(variables: Variable*) extends scas.power.growable.ArrayPowerProduct[Int](variables*) with scas.power.compact.PowerProduct {
  extension (x: Array[Int]) {
    override def get(i: Int) = {
      val p = (i << shift) + ((i + (1 << shift)) >> 5)
      val q = p >> 5
      if q < x.length then {
        val r = p & 31
        (x(q) >> r) & mask
      } else 0
    }
  }
}
