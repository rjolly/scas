package scas.polynomial.tree

import scala.compiletime.deferred
import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.util.Conversion
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

trait MultivariatePolynomial[C, S] extends TreePolynomial[C, Array[Int]] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, Array[Int]] with UniqueFactorizationDomain.Conv[Element[C, Array[Int]]] {
  def s: Seq[S]
  given Conversion[Variable][S] = deferred
  override given pp: PowerProduct[Array[Int]] = Lexicographic(0)(s*)
  given instance: MultivariatePolynomial[C, S] = this
}
