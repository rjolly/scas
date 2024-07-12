package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable

class PolynomialWithPrimitiveGCD[C](using UniqueFactorizationDomain[C])(s: Variable*) extends MultivariatePolynomial[C](s*) with scas.polynomial.PolynomialWithPrimitiveGCD[Element, C, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithPrimitiveGCD(using ring)(pp.variables*)
}
