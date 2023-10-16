package scas.power.splittable.impl

import scas.variable.Variable

trait PowerProduct[M] extends scas.power.impl.PowerProduct[M] {
  def take(n: Int) = newInstance(variables.take(n))
  def drop(n: Int) = newInstance(variables.drop(n))
  def newInstance(variables: Seq[Variable]): PowerProduct[M]
}
