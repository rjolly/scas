package scas.polynomial.tree

import scala.collection.SortedMap
import scas.Variable
import scas.polynomial.{TreePolynomial, PowerProduct, PolynomialOverUFD}
import scas.structure.Field
import UnivariatePolynomial.Element

class UnivariatePolynomial[C, @specialized(Int, Long) N](val ring: Field[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.UnivariatePolynomial[Element[C, N], C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object UnivariatePolynomial {
  def apply[C](ring: Field[C], s: Variable): UnivariatePolynomial[C, Int] = apply(ring, PowerProduct(s))
  def apply[C, @specialized(Int, Long) N](ring: Field[C], pp: PowerProduct[N]) = new UnivariatePolynomial(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(val factory: UnivariatePolynomial[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with PolynomialOverUFD.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2univariatePolynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: UnivariatePolynomial[C, N]) = factory(value)
  }
}
