package scas.polynomial.tree

import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSubresGCD[C](using UniqueFactorizationDomain[C])(variables: Variable*) extends MultivariatePolynomial(variables*) with scas.polynomial.ufd.PolynomialWithSubresGCD[Element, C, Int] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithSubresGCD(using ring)(pp.variables*)
}
