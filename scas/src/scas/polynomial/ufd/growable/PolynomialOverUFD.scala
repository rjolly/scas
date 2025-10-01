package scas.polynomial.ufd.growable

import scas.polynomial.GrowablePolynomial
import scas.variable.Variable

trait PolynomialOverUFD[T, C, M] extends GrowablePolynomial[T, C, M] with scas.polynomial.ufd.PolynomialOverUFD[T, C, M] {
  def extend(variables: Variable*): Unit = pp.extend(variables*)
}
