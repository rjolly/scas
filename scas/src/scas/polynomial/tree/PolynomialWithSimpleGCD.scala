package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSimpleGCD[C](using val ring: UniqueFactorizationDomain[C])(val s: Variable*) extends MultivariatePolynomial[C] with scas.polynomial.ufd.PolynomialWithSimpleGCD[Element, C, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithSimpleGCD(using ring)(pp.variables*)
}
