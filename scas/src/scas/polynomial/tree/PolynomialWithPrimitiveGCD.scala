package scas.polynomial.tree

import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithPrimitiveGCD[C](ring: UniqueFactorizationDomain[C])(variables: Variable*) extends MultivariatePolynomial(ring)(variables*) with scas.polynomial.ufd.PolynomialWithPrimitiveGCD[Element, C, Int] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithPrimitiveGCD(ring)(pp.variables*)
}
