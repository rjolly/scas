package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

abstract class MultivariatePolynomial[C, S : Conversion[Variable]](using val ring: UniqueFactorizationDomain[C])(s: S*) extends TreePolynomial[C, Array[Int]] with scas.polynomial.MultivariatePolynomial[Element, C, Array[Int]] with scas.structure.commutative.conversion.UniqueFactorizationDomain[Element[C, Array[Int]]] {
  given pp: PowerProduct[Array[Int]] = Lexicographic(0)(s.map(~_)*)
  given instance: MultivariatePolynomial[C, S] = this
}
