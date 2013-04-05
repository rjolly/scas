package scas.polynomial

import scala.collection.SortedMap
import scas.Implicits.{infixRingOps, infixPowerProductOps}
import TreePolynomial.Element

trait TreePolynomial[T <: Element[T, C, N], C, N] extends IterablePolynomial[T, C, N] {
  def apply(s: (Array[N], C)*) = apply(SortedMap(s: _*)(pp.reverse))
  def apply(value: SortedMap[Array[N], C]): T

  def plus(x: T, y: T) = apply((x.value /: y.value) { (l, r) =>
    val (t, b) = r
    val c = l.getOrElse(t, ring.zero) + b
    if (c.isZero) l - t else l + ((t, c))
  })

  override def iterator(x: T, m: Array[N]) = x.value.from(m).iterator

  override def subtract(x: T, m: Array[N], c: C, y: T) = apply((x.value /: y.value) { (l, r) =>
    val (s, a) = r
    val (sm, ac) = (s * m, a * c)
    val cc = l.getOrElse(sm, ring.zero) - ac
    if (cc.isZero) l - sm else l + ((sm, cc))
  })

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = apply((zero.value /: x.value) { (l, r) =>
    val (s, a) = r
    val (m, c) = f(s, a)
    if (c.isZero) l else l + ((m, c))
  })

  override def sort(x: T) = x
}

object TreePolynomial {
  trait Element[T <: Element[T, C, N], C, N] extends IterablePolynomial.Element[T, C, N] { this: T =>
    val factory: TreePolynomial[T, C, N]
    val value: SortedMap[Array[N], C]
  }
}
