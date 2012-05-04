package scas.polynomial.tree

import scala.collection.SortedMap
import scas.Variable
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.Field
import scas.module.Module
import UnivariatePolynomial.Element

class UnivariatePolynomial[C, @specialized(Int, Long) N](val ring: Field[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.UnivariatePolynomial[Element[C, N], C, N] {
  def apply(x: Element[C, N], syzygy: Module.Element[Polynomial.Element[C, N]]) = apply(x.value, syzygy)
  def apply(value: SortedMap[Array[N], C], syzygy: Module.Element[Polynomial.Element[C, N]]) = new Element(value, syzygy)(this)
  def apply(value: SortedMap[Array[N], C]) = apply(value, module.zero)
  def fromSyzygy(x: Polynomial.Element[C, N]) = apply(x.value)
}

object UnivariatePolynomial {
  def apply[C](ring: Field[C], s: Variable): UnivariatePolynomial[C, Int] = apply(ring, PowerProduct(s))
  def apply[C, @specialized(Int, Long) N](ring: Field[C], pp: PowerProduct[N]) = new UnivariatePolynomial(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C], val syzygy: Module.Element[Polynomial.Element[C, N]])(val factory: UnivariatePolynomial[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with scas.polynomial.UnivariatePolynomial.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: UnivariatePolynomial[C, N]) = factory(value)
}
