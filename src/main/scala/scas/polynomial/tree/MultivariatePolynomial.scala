package scas.polynomial.tree

import scala.collection.SortedMap
import scas.polynomial.{TreePolynomial, PowerProduct, PolynomialOverUFD}
import scas.structure.{UniqueFactorizationDomain, Field}
import MultivariatePolynomial.Element

trait MultivariatePolynomial[C, @specialized(Int, Long) N] extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.MultivariatePolynomial[Element, C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object MultivariatePolynomial {
  def withSimpleGCD[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithSimpleGCD(ring, pp)
  def withPrimitiveGCD[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithPrimitiveGCD(ring, pp)
  def apply[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithSubresGCD(ring, pp)
  def apply[C, @specialized(Int, Long) N](ring: Field[C], pp: PowerProduct[N]) = new PolynomialWithMonicGCD(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(val factory: MultivariatePolynomial[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with PolynomialOverUFD.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2multivariatePolynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: MultivariatePolynomial[C, N]) = factory(value)
  }
}
