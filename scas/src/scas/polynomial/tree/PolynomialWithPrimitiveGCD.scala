package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.TreePolynomial.Element

class PolynomialWithPrimitiveGCD[C, S : Conversion[Variable]](using val ring: UniqueFactorizationDomain[C])(s: S*) extends MultivariatePolynomial[C, S](s*) with scas.polynomial.ufd.PolynomialWithPrimitiveGCD[Element, C, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithPrimitiveGCD(using ring)(pp.variables*)
}
