package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSimpleGCD[C, S : Conversion[Variable]](using UniqueFactorizationDomain[C])(val s: S*) extends MultivariatePolynomial[C, S] with scas.polynomial.ufd.PolynomialWithSimpleGCD[Element, C, Array[Int]] {
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithSimpleGCD(using ring)(pp.variables*)
}
