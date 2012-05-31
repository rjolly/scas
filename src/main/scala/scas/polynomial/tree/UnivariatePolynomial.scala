package scas.polynomial.tree

import scala.collection.SortedMap
import scas.Variable
import scas.polynomial.{TreePolynomial, PowerProduct, Module}
import scas.structure.Field
import UnivariatePolynomial.Element

class UnivariatePolynomial[C, @specialized(Int, Long) N](val ring: Field[C], val pp: PowerProduct[N], val module: Module[Polynomial.Element[C, N], C, N])(implicit val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.UnivariatePolynomialWithSyzygy[Polynomial.Element, Element[C, N], C, N] {
  def apply(x: Element[C, N], element: Module.Element[Polynomial.Element[C, N]]) = apply(x.value, element)
  def apply(value: SortedMap[Array[N], C], element: Module.Element[Polynomial.Element[C, N]]) = new Element(value, element)(this)
  def apply(value: SortedMap[Array[N], C]) = apply(value, module.zero)
  def fromPolynomial(x: Polynomial.Element[C, N]) = apply(x.value)
}

object UnivariatePolynomial {
  def apply[C](ring: Field[C], s: Variable): UnivariatePolynomial[C, Int] = apply(ring, PowerProduct(s))
  def apply[C, @specialized(Int, Long) N](ring: Field[C], pp: PowerProduct[N]) = new UnivariatePolynomial(ring, pp, Module("e", 2, Polynomial(ring, pp)))

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C], val element: Module.Element[Polynomial.Element[C, N]])(val factory: UnivariatePolynomial[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with scas.polynomial.UnivariatePolynomialWithSyzygy.Element[Polynomial.Element, Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2univariatePolynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: UnivariatePolynomial[C, N]) = factory(value)
  }
}
