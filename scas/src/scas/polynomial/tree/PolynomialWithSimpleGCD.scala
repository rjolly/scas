package scas.polynomial.tree

import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSimpleGCD[C](ring: UniqueFactorizationDomain[C])(variables: Variable*) extends MultivariatePolynomial(ring)(variables*) with scas.polynomial.ufd.PolynomialWithSimpleGCD[Element, C, Int] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithSimpleGCD(ring)(pp.variables*)
}
