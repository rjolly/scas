package scas.polynomial.tree

import scas.power.splitable.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSubresGCD[C](using UniqueFactorizationDomain[C])(val variables: Variable*) extends MultivariatePolynomial[C] with scas.polynomial.ufd.PolynomialWithSubresGCD[Element, C, Array[Int]] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithSubresGCD(using ring)(pp.variables*)
}
