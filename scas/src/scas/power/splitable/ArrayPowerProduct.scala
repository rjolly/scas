package scas.power.splitable

import scas.variable.Variable

trait ArrayPowerProduct[N] extends scas.power.ArrayPowerProduct[N] {
  def take(n: Int) = newInstance(variables.take(n)*)
  def drop(n: Int) = newInstance(variables.drop(n)*)
  def newInstance(variables: Variable*): ArrayPowerProduct[N]
}
