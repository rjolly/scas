package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable

class PolynomialWithSimpleGCD[C](using UniqueFactorizationDomain[C])(s: Variable*) extends MultivariatePolynomial[C](s*) with scas.polynomial.PolynomialWithSimpleGCD[Element, C, Array[Int]] {
  override given instance: MultivariatePolynomial[C] = this
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithSimpleGCD(using ring)(pp.variables*)
}
