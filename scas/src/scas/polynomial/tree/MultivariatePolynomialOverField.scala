package scas.polynomial.tree

import scas.power.splitable.PowerProduct
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class MultivariatePolynomialOverField[C](using Field[C])(val variables: Variable*) extends MultivariatePolynomial[C] with scas.polynomial.ufd.MultivariatePolynomialOverField[Element, C, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithSubresGCD(using ring)(pp.variables*)
}
