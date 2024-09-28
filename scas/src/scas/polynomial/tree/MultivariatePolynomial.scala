package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

abstract class MultivariatePolynomial[C, S : Conversion[Variable]](s: S*) extends TreePolynomial[C, Array[Int]] with scas.polynomial.MultivariatePolynomial[Element, C, Array[Int]] {
  given pp: PowerProduct[Array[Int]] = Lexicographic(0)(s*)
  given instance: MultivariatePolynomial[C, S] = this
}
