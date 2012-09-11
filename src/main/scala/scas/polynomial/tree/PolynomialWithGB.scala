package scas.polynomial.tree

import scala.collection.SortedMap
import scas.Variable
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.UniqueFactorizationDomain
import PolynomialWithGB.Element

class PolynomialWithGB[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.PolynomialWithGB[Element[C, N], C, N] {
  def apply(x: Element[C, N], index: Int) = apply(x.value, index)
  def apply(value: SortedMap[Array[N], C], index: Int) = new Element(value, index)(this)
  def apply(value: SortedMap[Array[N], C]) = apply(value, 0)
}

object PolynomialWithGB {
  def apply[C](ring: UniqueFactorizationDomain[C], variables: Variable*): PolynomialWithGB[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithGB(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C], val index: Int)(val factory: PolynomialWithGB[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with scas.polynomial.PolynomialWithGB.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomialWithGB[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: PolynomialWithGB[C, N]) = factory(value)
  }
}
