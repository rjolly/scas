package scas.polynomial

import IterablePolynomial.Element

trait IterablePolynomial[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] {
  override def isZero(x: T) = x.value.isEmpty

  def iterator(x: T) = x.value.iterator

  override def toSeq(x: T) = x.value.toSeq

  def size(x: T) = x.value.size

  def head(x: T) = x.value.head

  def last(x: T) = x.value.last
}

object IterablePolynomial {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: IterablePolynomial[T, C, N]
    val value: Iterable[(Array[N], C)]
  }
}
