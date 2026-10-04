package scas.polynomial.tree

import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithPrimitiveGCD[C](using UniqueFactorizationDomain[C])(variables: Variable*) extends MultivariatePolynomial(variables*) with scas.polynomial.ufd.PolynomialWithPrimitiveGCD[Element, C, Int] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithPrimitiveGCD(using ring)(pp.variables*)
}
