package scas.polynomial

import scala.compiletime.deferred
import scas.power.growable.PowerProduct
import scas.variable.Variable

trait GrowablePolynomial[T, C, M] extends Polynomial[T, C, M] {
  given pp: PowerProduct[M] = deferred
  def extend(variables: Variable*): Unit = pp.extend(variables*)
}
