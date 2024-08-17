package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.TreePolynomial.Element

class MultivariatePolynomialOverField[C, S : Conversion[Variable]](using val ring: Field[C])(s: S*) extends MultivariatePolynomial[C, S](s*) with scas.polynomial.MultivariatePolynomialOverField[Element, C, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithSubresGCD(using ring)(pp.variables*)
}
