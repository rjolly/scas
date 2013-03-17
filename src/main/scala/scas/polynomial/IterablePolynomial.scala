package scas.polynomial

import scas.Implicits.infixOrderingOps
import IterablePolynomial.Element

trait IterablePolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  override def isZero(x: T) = x.value.isEmpty

  def iterator(x: T) = x.value.iterator

  def iterator(x: T, m: Array[N]) = x.value.dropWhile({ r =>
    val (s, _) = r
    s > m
  }).iterator

  def reverseIterator(x: T) = x.value.toSeq.reverseIterator

  def size(x: T) = x.value.size

  def head(x: T) = x.value.head

  def last(x: T) = x.value.last
}

object IterablePolynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: IterablePolynomial[T, C, N]
    val value: Iterable[(Array[N], C)]
  }
}
