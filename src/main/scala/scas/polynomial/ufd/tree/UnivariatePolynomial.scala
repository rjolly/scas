package scas.polynomial.ufd.tree

import scala.collection.SortedMap
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.Field
import scas.module.Module
import UnivariatePolynomial.Element

class UnivariatePolynomial[C, @specialized(Int, Long) N](val ring: Field[C], val pp: PowerProduct[N], val module: Module[Element[C, N]])(implicit val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.ufd.UnivariatePolynomial[Element[C, N], C, N] {
  def this(ring: Field[C], pp: PowerProduct[N])(implicit cm: ClassManifest[Element[C, N]]) = this(ring, pp, Module("e", 0, null))
  def ps(dimension: Int) = new UnivariatePolynomial(ring, pp, Module("e", dimension, this))
  def apply(x: Element[C, N], syzygy: Module.Element[Element[C, N]]) = new Element(x.value, syzygy)(this)
  def apply(value: SortedMap[Array[N], C]) = new Element(value, module.zero)(this)
}

object UnivariatePolynomial {
  def apply[C, @specialized(Int, Long) N](ring: Field[C], pp: PowerProduct[N]) = new UnivariatePolynomial(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C], val syzygy: Module.Element[Element[C, N]])(val factory: UnivariatePolynomial[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with scas.polynomial.ufd.UnivariatePolynomial.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: UnivariatePolynomial[C, N]) = factory(value)
}
