package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class UnivariatePolynomial[C](using Field[C])(variable: Variable) extends TreePolynomial[C, Array[Int]] with scas.polynomial.ufd.UnivariatePolynomial[Element[C, Array[Int]], C, Array[Int]] with UniqueFactorizationDomain.Conv[Element[C, Array[Int]]] {
  override given pp: PowerProduct[Array[Int]] = Lexicographic.inlined(0)(variable)
  given instance: UnivariatePolynomial[C] = this
}

object UnivariatePolynomial {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S) = new UnivariatePolynomial(using ring)(~s)
}
