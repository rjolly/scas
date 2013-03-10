package scas.polynomial

import scala.collection.SortedMap
import scas.Implicits.infixRingOps
import TreePolynomial.Element

trait TreePolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  override def isZero(x: T) = x.value.isEmpty
  def apply(s: (Array[N], C)*) = apply(SortedMap(s: _*)(pp.ordering.reverse))
  def apply(value: SortedMap[Array[N], C]): T

  def iterator(x: T) = x.value.iterator

  def iterator(x: T, m: Array[N]) = x.value.from(m).iterator

  def reverseIterator(x: T) = x.value.toSeq.reverseIterator

  def size(x: T) = x.value.size

  def head(x: T) = x.value.head

  def last(x: T) = x.value.last

  def combine(x: T, y: T, f: (C, C) => C) = apply((x.value /: y.value) { (l, r) =>
    val (s, a) = r
    val c = f(l.getOrElse(s, ring.zero), a)
    if (c.isZero) l - s else l + ((s, c))
  })

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = apply((zero.value /: x.value) { (l, r) =>
    val (s, a) = r
    val (m, c) = f(s, a)
    if (c.isZero) l else l + ((m, c))
  })

  def sort(x: T) = x
}

object TreePolynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: TreePolynomial[T, C, N]
    val value: SortedMap[Array[N], C]
  }
}
