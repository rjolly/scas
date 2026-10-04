package scas.polynomial.tree

import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSimpleGCD[C](using UniqueFactorizationDomain[C])(variables: Variable*) extends MultivariatePolynomial(variables*) with scas.polynomial.ufd.PolynomialWithSimpleGCD[Element, C, Int] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithSimpleGCD(using ring)(pp.variables*)
}
