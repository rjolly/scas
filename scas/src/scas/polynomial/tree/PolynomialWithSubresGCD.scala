package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSubresGCD[C, S : Conversion[Variable]](using UniqueFactorizationDomain[C])(val s: S*) extends MultivariatePolynomial[C, S] with scas.polynomial.ufd.PolynomialWithSubresGCD[Element, C, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithSubresGCD(using ring)(pp.variables*)
}
