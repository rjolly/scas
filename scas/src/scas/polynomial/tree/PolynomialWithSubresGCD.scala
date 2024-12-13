package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSubresGCD[C](using val ring: UniqueFactorizationDomain[C])(s: Variable*) extends MultivariatePolynomial[C](s*) with scas.polynomial.ufd.PolynomialWithSubresGCD[Element, C, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithSubresGCD(using ring)(pp.variables*)
}
