package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.conversion.UniqueFactorizationDomain
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.TreePolynomial
import MultivariatePolynomial.Element

abstract class MultivariatePolynomial[C, S : Conversion[Variable]](s: S*) extends TreePolynomial[C, Array[Int]] with scas.polynomial.MultivariatePolynomial[TreePolynomial.Element, C, Array[Int]] with UniqueFactorizationDomain[Element[C]] {
  given pp: PowerProduct[Array[Int]] = Lexicographic(0)(s*)
  given instance: MultivariatePolynomial[C, S] = this
}

object MultivariatePolynomial {
  type Element[C] = TreePolynomial.Element[C, Array[Int]]
}
