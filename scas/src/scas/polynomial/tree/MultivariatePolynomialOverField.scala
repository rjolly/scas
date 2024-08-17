package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.PolynomialOverField
import scas.polynomial.TreePolynomial.Element

class MultivariatePolynomialOverField[C, S : Conversion[Variable]](using val ring: Field[C])(s: S*) extends MultivariatePolynomial[C, S](s*) with scas.polynomial.PolynomialWithSubresGCD[Element, C, Array[Int]] with PolynomialOverField[Element[C, Array[Int]], C, Array[Int]] {
  extension (x: Element[C, Array[Int]]) def modInverse(mods: Element[C, Array[Int]]*) = ???
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[Array[Int]]) => new PolynomialWithSubresGCD(using ring)(pp.variables*)
}
