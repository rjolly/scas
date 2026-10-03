package scas.polynomial.tree

import scas.power.splitable.ArrayPowerProduct
import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial.Element

class MultivariatePolynomialOverField[C : Field, N : ArrayPowerProduct] extends MultivariatePolynomial[C, N] with scas.polynomial.ufd.MultivariatePolynomialOverField[Element, C, N] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithSubresGCD(using ring, pp)
}
