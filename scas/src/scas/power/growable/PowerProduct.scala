package scas.power.growable

import scas.variable.Variable

trait PowerProduct[M] extends scas.power.PowerProduct[M] {
  def extend(variables: Variable*): Unit
}
