package scas.polynomial
package mutable

import java.util.{SortedMap, TreeMap}
import scala.collection.convert.WrapAsScala
import scas.Implicits.{infixRingOps, infixPowerProductOps}
import TreePolynomial.Element
import WrapAsScala.mapAsScalaMap

trait TreePolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  override def isZero(x: T) = x.value.isEmpty
  def apply(s: (Array[N], C)*) = {
    val l = new TreeMap[Array[N], C](pp.ordering.reverse)
    s.foreach { l += _ }
    apply(l)
  }
  def apply(value: SortedMap[Array[N], C]): T

  override def add(x: T, m: Array[N], c: C, y: T) = {
    val l = x.value
    y.value.foreach { r =>
      val (t, b) = r
      val (s, a) = (t * m, b * c)
      val cc = l.getOrElse(s, ring.zero) + a
      if (cc.isZero) l -= s else l += ((s, cc))
    }
    x
  }

  def iterator(x: T) = x.value.iterator

  def iterator(x: T, m: Array[N]) = x.value.tailMap(m).iterator

  def reverseIterator(x: T) = x.value.toSeq.reverseIterator

  def size(x: T) = x.value.size

  def head(x: T) = { val a = headPowerProduct(x) ; (a, x.value.get(a)) }

  override def headPowerProduct(x: T) = x.value.firstKey

  def last(x: T) = { val a = lastPowerProduct(x) ; (a, x.value.get(a)) }

  def lastPowerProduct(x: T) = x.value.lastKey

  def combine(x: T, y: T, f: (C, C) => C) = {
    val l = new TreeMap(x.value)
    y.value.foreach { r =>
      val (s, a) = r
      val c = f(l.getOrElse(s, ring.zero), a)
      if (c.isZero) l -= s else l += ((s, c))
    }
    apply(l)
  }

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = {
    val l = zero.value
    x.value.foreach { r =>
      val (s, a) = r
      val (m, c) = f(s, a)
      if (c.isZero) () else l += ((m, c))
    }
    apply(l)
  }

  def sort(x: T) = x
}

object TreePolynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: TreePolynomial[T, C, N]
    val value: SortedMap[Array[N], C]
  }
}
