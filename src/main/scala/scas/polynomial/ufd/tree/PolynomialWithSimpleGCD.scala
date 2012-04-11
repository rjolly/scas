package scas.polynomial.ufd.tree

import scala.collection.SortedMap
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.UniqueFactorizationDomain
import scas.polynomial.ufd.PolynomialOverUFD
import PolynomialWithSimpleGCD.Element

class PolynomialWithSimpleGCD[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.ufd.PolynomialWithSimpleGCD[Element, C, N] {
  def split = PolynomialWithSimpleGCD(PolynomialWithSimpleGCD(ring, pp.take(location)), pp.drop(location))
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object PolynomialWithSimpleGCD {
  def apply[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithSimpleGCD(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(val factory: PolynomialWithSimpleGCD[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with PolynomialOverUFD.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: PolynomialWithSimpleGCD[C, N]) = factory(value)
}
