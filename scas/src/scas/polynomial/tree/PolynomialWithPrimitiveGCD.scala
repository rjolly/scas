package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithPrimitiveGCD[C](using UniqueFactorizationDomain[C])(val s: Variable*) extends MultivariatePolynomial[C] with scas.polynomial.ufd.PolynomialWithPrimitiveGCD[Element, C, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithPrimitiveGCD(using ring)(pp.variables*)
}
