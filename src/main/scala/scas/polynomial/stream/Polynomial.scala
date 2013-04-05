package scas.polynomial
package stream

import scas.immutable.Stream
import scas.Variable
import scas.power.PowerProduct
import scas.structure.Ring
import Polynomial.Element

trait Polynomial[C, N] extends StreamPolynomial[Element[C, N], C, N] {
  def apply(value: Stream[(Array[N], C)]) = new Element(value)(this)
}

object Polynomial {
  def apply[C](ring: Ring[C], variables: Variable*): Polynomial[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, N](ring: Ring[C], pp: PowerProduct[N]) = new PolynomialImpl(ring, pp)

  class Element[C, N](val value: Stream[(Array[N], C)])(val factory: Polynomial[C, N]) extends StreamPolynomial.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomial[D, C, N](value: D)(implicit f: D => C, factory: Polynomial[C, N]) = factory(value)
  }
}
