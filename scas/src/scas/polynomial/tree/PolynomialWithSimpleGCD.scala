package scas.polynomial.tree

import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSimpleGCD[C](using UniqueFactorizationDomain[C])(val variables: Variable*) extends MultivariatePolynomial[C] with scas.polynomial.ufd.PolynomialWithSimpleGCD[Element, C, Array[Int]] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithSimpleGCD(using ring)(pp.variables*)
}
