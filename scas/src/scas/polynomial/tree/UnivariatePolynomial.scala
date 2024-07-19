package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.conversion.UniqueFactorizationDomain
import scas.structure.commutative.Field
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class UnivariatePolynomial[C, S : Conversion[Variable]](using val ring: Field[C])(s: S*) extends TreePolynomial[C, Array[Int]] with scas.polynomial.UnivariatePolynomial[Element[C, Array[Int]], C, Array[Int]] with UniqueFactorizationDomain[Element[C, Array[Int]]] {
  given pp: PowerProduct[Array[Int]] = Lexicographic.inlined(0)(s*)
  given instance: UnivariatePolynomial[C, S] = this
}
