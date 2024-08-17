package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.TreePolynomial.Element

class BooleanPolynomial[S : Conversion[Variable]](s: S*) extends MultivariatePolynomial[Boolean, S](s*) with scas.polynomial.BooleanPolynomial[Element, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithSubresGCD(using ring)(pp.variables*)
}
