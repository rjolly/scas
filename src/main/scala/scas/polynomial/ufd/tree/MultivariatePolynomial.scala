package scas.polynomial.ufd.tree

import scala.collection.SortedMap
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.polynomial.ufd.{PolynomialOverUFD, PolynomialWithSimpleGCD, PolynomialWithPrimitiveGCD, PolynomialWithSubresGCD}
import scas.structure.UniqueFactorizationDomain
import MultivariatePolynomial.Element

abstract class MultivariatePolynomial[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object MultivariatePolynomial {
  def apply[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]): MultivariatePolynomial[C, N] = new MultivariatePolynomial(ring, pp) with PolynomialWithSimpleGCD[Element, C, N] {
    def split = MultivariatePolynomial(MultivariatePolynomial(ring, pp.take(location)), pp.drop(location))
  }
  def withPrimitiveGCD[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]): MultivariatePolynomial[C, N] = new MultivariatePolynomial(ring, pp) with PolynomialWithPrimitiveGCD[Element, C, N] {
    def split = withPrimitiveGCD(withPrimitiveGCD(ring, pp.take(location)), pp.drop(location))
  }
  def withSubresGCD[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]): MultivariatePolynomial[C, N] = new MultivariatePolynomial(ring, pp) with PolynomialWithSubresGCD[Element, C, N] {
    def split = withSubresGCD(withSubresGCD(ring, pp.take(location)), pp.drop(location))
  }

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(val factory: MultivariatePolynomial[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with PolynomialOverUFD.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: MultivariatePolynomial[C, N]) = factory(value)
}
