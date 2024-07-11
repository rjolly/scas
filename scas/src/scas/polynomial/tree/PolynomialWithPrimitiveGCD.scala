package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable

class PolynomialWithPrimitiveGCD[C](using UniqueFactorizationDomain[C])(s: Variable*) extends MultivariatePolynomial[C, Array[Int]] with scas.polynomial.PolynomialWithPrimitiveGCD[Element, C, Array[Int]] {
  override given pp: PowerProduct[Array[Int]] = Lexicographic[Int](s*)
  override given instance: MultivariatePolynomial[C, Array[Int]] = this
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithPrimitiveGCD(using ring)(pp.variables*)
}
