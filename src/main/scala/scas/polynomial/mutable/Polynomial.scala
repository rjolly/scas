package scas.polynomial.mutable

import java.util.SortedMap
import scas.Variable
import scas.polynomial.{TreeMutablePolynomial, PowerProduct}
import scas.structure.Ring
import Polynomial.Element

trait Polynomial[C, @specialized(Int, Long) N] extends TreeMutablePolynomial[Element[C, N], C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object Polynomial {
  def apply[C](ring: Ring[C], variables: Variable*): Polynomial[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N]) = new PolynomialImpl(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(val factory: Polynomial[C, N]) extends TreeMutablePolynomial.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: Polynomial[C, N]) = factory(value)
  }
}
