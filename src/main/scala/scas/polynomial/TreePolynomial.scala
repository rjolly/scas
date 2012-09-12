package scas.polynomial

import scala.collection.SortedMap
import scas.Implicits.infixRingOps
import TreePolynomial.Element

trait TreePolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  override def isZero(x: T) = x.value.isEmpty
  def plus(x: T, y: T) = apply((x.value /: iterator(y)) { (l, r) =>
    val (s, a) = r
    val c = l.getOrElse(s, ring.zero) + a
    if (c.isZero) l - s else l + ((s, c))
  })
  def minus(x: T, y: T) = apply((x.value /: iterator(y)) { (l, r) =>
    val (s, a) = r
    val c = l.getOrElse(s, ring.zero) - a
    if (c.isZero) l - s else l + ((s, c))
  })
  def apply(s: (Array[N], C)*) = apply(SortedMap(s: _*)(pp.ordering.reverse))
  def apply(value: SortedMap[Array[N], C]): T

  def iterator(x: T) = x.value.iterator

  def iterator(x: T, m: Array[N]) = x.value.from(m).iterator

  def reverseIterator(x: T) = toSeq(x).reverseIterator

  def toSeq(x: T) = x.value.toSeq

  def size(x: T) = x.value.size

  def head(x: T) = x.value.head

  def last(x: T) = x.value.last

  override def map(x: T, f: (Array[N], C) => (Array[N], C)) = apply((zero.value /: iterator(x)) { (l, r) =>
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
