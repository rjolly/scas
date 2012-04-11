package scas.polynomial

import scala.collection.SortedMap
import scas.Implicits.infixRingOps
import TreePolynomial.Element

trait TreePolynomial[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] {
  override def zero = apply(SortedMap.empty[Array[N], C](pp.ordering.reverse))
  override def signum(x: T): Int = {
    val it = iterator(x)
    if (!it.hasNext) return 0
    val (a, b) = it.next ; ring.signum(b)
  }
  def apply(x: T) = apply((zero.value /: x.value.iterator) { (l, r) =>
    val (a, b) = r
    val (m, c) = (pp.converter(x.factory.variables)(a), ring(b))
    if (c.isZero) l else l.updated(m, c)
  })
  override def isZero(x: T) = x.value isEmpty
  def plus(x: T, y: T) = apply((x.value /: y.value.iterator) { (l, r) =>
    val (a, b) = r
    val c = l.getOrElse(a, ring.zero) + b
    if (c.isZero) l - a else l.updated(a, c)
  })
  def minus(x: T, y: T) = apply((x.value /: y.value.iterator) { (l, r) =>
    val (a, b) = r
    val c = l.getOrElse(a, ring.zero) - b
    if (c.isZero) l - a else l.updated(a, c)
  })
  def apply(value: C) = apply(if(value.isZero) zero.value else zero.value + (pp.one -> value))
  def fromPowerProduct(value: Array[N]) = apply(zero.value + (value -> ring.one))
  def apply(value: SortedMap[Array[N], C]): T

  def iterator(x: T) = x.value.iterator

  override def headPowerProduct(x: T) = x.value.firstKey

  def map(w: T, f: (Array[N], C) => (Array[N], C)) = apply((zero.value /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val (m, c) = f(a, b)
    if (c.isZero) l else l.updated(m, c)
  })
}

object TreePolynomial {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: TreePolynomial[T, C, N]
    val value: SortedMap[Array[N], C]
  }
}
