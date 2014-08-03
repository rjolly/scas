package scas.polynomial

import scas.Variable
import scas.power.growable.PowerProduct
import Polynomial.Element

trait GrowablePolynomial[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] {
  var pp: PowerProduct[N]
  def +=(variable: Variable): Unit = pp += variable
}
