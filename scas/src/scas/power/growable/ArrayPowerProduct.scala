package scas.power.growable

import scas.math.Numeric
import scas.variable.Variable

trait ArrayPowerProduct[N : Numeric as numeric] extends scas.power.ArrayPowerProduct[N] with PowerProduct[Array[N]] {
  def newInstance(variables: Variable*): ArrayPowerProduct[N]
}
