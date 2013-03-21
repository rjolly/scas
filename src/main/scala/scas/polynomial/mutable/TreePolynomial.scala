package scas.polynomial
package mutable

import java.util.{SortedMap, TreeMap}
import scala.collection.convert.WrapAsScala
import scas.Implicits.{infixRingOps, infixPowerProductOps}
import TreePolynomial.Element
import WrapAsScala.mapAsScalaMap

trait TreePolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  override def isZero(x: T) = x.value.isEmpty
  def clone(x: T) = apply(new TreeMap(x.value))
  def apply(s: (Array[N], C)*) = {
    val l = new TreeMap[Array[N], C](pp.ordering.reverse)
    s.foreach { l += _ }
    apply(l)
  }
  def apply(value: SortedMap[Array[N], C]): T

  def plus(x: T, y: T) = add(clone(x), pp.one, ring.one, y)
  override def minus(x: T, y: T) = add(clone(x), pp.one, -ring.one, y)

  override def add(x: T, m: Array[N], c: C, y: T) = {
    val l = x.value
    y.value.foreach { r =>
      val (t, b) = r
      val (tm, bc) = (t * m, b * c)
      val cc = l.getOrElse(tm, ring.zero) + bc
      if (cc.isZero) l -= tm else l += ((tm, cc))
    }
    x
  }

  def iterator(x: T) = x.value.iterator

  def iterator(x: T, m: Array[N]) = x.value.tailMap(m).iterator

  override def toSeq(x: T) = x.value.toSeq

  def size(x: T) = x.value.size

  def head(x: T) = { val a = headPowerProduct(x) ; (a, x.value.get(a)) }

  override def headPowerProduct(x: T) = x.value.firstKey

  def last(x: T) = { val a = lastPowerProduct(x) ; (a, x.value.get(a)) }

  def lastPowerProduct(x: T) = x.value.lastKey

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = {
    val l = zero.value
    x.value.foreach { r =>
      val (s, a) = r
      val (m, c) = f(s, a)
      if (c.isZero) () else l += ((m, c))
    }
    apply(l)
  }

  override def sort(x: T) = x
}

object TreePolynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: TreePolynomial[T, C, N]
    val value: SortedMap[Array[N], C]
  }
}
