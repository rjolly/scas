package scas.list

import scala.collection.immutable.List
import scas.Variable
import scas.polynomial.{ListPolynomial, PowerProduct}
import scas.structure.Ring
import Polynomial.Element

trait Polynomial[C, @specialized(Int, Long) N] extends ListPolynomial[Element[C, N], C, N] {
  def apply(value: List[(Array[N], C)]) = new Element(value)(this)
}

object Polynomial {
  def apply[C](ring: Ring[C], variables: Variable*): Polynomial[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N]) = new PolynomialImpl(ring, pp)
  def parallel[C](ring: Ring[C], variables: Variable*): Polynomial[C, Int] = parallel(ring, PowerProduct(variables: _*))
  def parallel[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N]) = new ParallelPolynomial(ring, pp)
  def stream[C](ring: Ring[C], variables: Variable*): Polynomial[C, Int] = stream(ring, PowerProduct(variables: _*))
  def stream[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N]) = new StreamPolynomial(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: List[(Array[N], C)])(val factory: Polynomial[C, N]) extends ListPolynomial.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: Polynomial[C, N]) = factory(value)
  }
}
