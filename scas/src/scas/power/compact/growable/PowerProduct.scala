package scas.power.compact.growable

import scas.variable.Variable

abstract class PowerProduct(val shift: Int)(variables: Variable*) extends scas.power.growable.ArrayPowerProduct[Int](variables*) with scas.power.compact.PowerProduct {
  override def length = scala.math.max(super.length, 1)
}
