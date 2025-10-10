package scas.power.splitable

import scas.variable.Variable

trait PowerProduct[M] extends scas.power.PowerProduct[M] {
  extension (x: M) def convert(from: PowerProduct[M]): M
  def take(n: Int) = newInstance(variables.take(n)*)
  def drop(n: Int) = newInstance(variables.drop(n)*)
  def newInstance(variables: Variable*): PowerProduct[M]
}
