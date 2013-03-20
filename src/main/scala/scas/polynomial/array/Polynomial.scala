package scas.polynomial
package array

import scala.reflect.ClassTag
import scas.Variable
import scas.structure.Ring
import Polynomial.Element

trait Polynomial[C, @specialized(Int, Long) N] extends ArrayPolynomial[Element[C, N], C, N] {
  def apply(value: (Array[C], Array[N])) = new Element(value)(this)
}

object Polynomial {
  def apply[C](ring: Ring[C], variables: Variable*)(implicit cm: ClassTag[Element[C, Int]], cm1: ClassTag[C]): Polynomial[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N])(implicit cm: ClassTag[Element[C, N]], cm1: ClassTag[C], cm2: ClassTag[N]) = new PolynomialImpl(ring, pp)
  def parallel[C](ring: Ring[C], variables: Variable*)(implicit cm: ClassTag[Element[C, Int]], cm1: ClassTag[C]): Polynomial[C, Int] = parallel(ring, PowerProduct(variables: _*))
  def parallel[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N])(implicit cm: ClassTag[Element[C, N]], cm1: ClassTag[C], cm2: ClassTag[N]) = new ParallelPolynomial(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: (Array[C], Array[N]))(val factory: Polynomial[C, N]) extends ArrayPolynomial.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: Polynomial[C, N]) = factory(value)
  }
}
