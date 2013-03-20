package scas.polynomial

import scala.collection.SortedMap
import scas.Implicits.{infixRingOps, infixPowerProductOps}
import TreePolynomial.Element

trait TreePolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends IterablePolynomial[T, C, N] {
  def apply(s: (Array[N], C)*) = apply(SortedMap(s: _*)(pp.ordering.reverse))
  def apply(value: SortedMap[Array[N], C]): T

  def plus(x: T, y: T) = add(x, pp.one, ring.one, y)
  override def minus(x: T, y: T) = add(x, pp.one, -ring.one, y)

  override def add(x: T, m: Array[N], c: C, y: T) = apply((x.value /: y.value) { (l, r) =>
    val (t, b) = r
    val (tm, bc) = (t * m, b * c)
    val cc = l.getOrElse(tm, ring.zero) + bc
    if (cc.isZero) l - tm else l + ((tm, cc))
  })

  override def iterator(x: T, m: Array[N]) = x.value.from(m).iterator

  override def subtract(x: T, m: Array[N], a: C, y: T, b: C) = add(multiply(x, b), m, -a, y)

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = apply((zero.value /: x.value) { (l, r) =>
    val (s, a) = r
    val (m, c) = f(s, a)
    if (c.isZero) l else l + ((m, c))
  })

  override def sort(x: T) = x
}

object TreePolynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends IterablePolynomial.Element[T, C, N] { this: T =>
    val factory: TreePolynomial[T, C, N]
    val value: SortedMap[Array[N], C]
  }
}
