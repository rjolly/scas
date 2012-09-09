package scas.polynomial

import java.util.{SortedMap, TreeMap}
import scala.collection.convert.WrapAsScala
import scas.Implicits.{infixRingOps, infixPowerProductOps}
import TreeMutablePolynomial.Element
import WrapAsScala.mapAsScalaMap

trait TreeMutablePolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  override def isZero(x: T) = x.value.isEmpty
  def apply(s: (Array[N], C)*) = {
    val l = new TreeMap[Array[N], C](pp.ordering.reverse)
    s.foreach { l += _ }
    apply(l)
  }
  def apply(value: SortedMap[Array[N], C]): T

  override def times(x: T, y: T) = {
    val l = zero.value
    iterator(y).foreach { r =>
      val (t, b) = r
      iterator(x).foreach { q =>
        val (s, a) = q
        val m = s * t
        val c = l.getOrElse(m, ring.zero) + a * b
        if (c.isZero) l -= m else l += ((m, c))
      }
    }
    apply(l)
  }

  def iterator(x: T) = x.value.iterator

  def iterator(x: T, m: Array[N]) = x.value.tailMap(m).iterator

  def reverseIterator(x: T) = toSeq(x).reverseIterator

  def toSeq(x: T) = x.value.toSeq

  def size(x: T) = x.value.size

  def head(x: T) = { val a = headPowerProduct(x) ; (a, x.value.get(a)) }

  override def headPowerProduct(x: T) = x.value.firstKey

  def last(x: T) = { val a = lastPowerProduct(x) ; (a, x.value.get(a)) }

  def lastPowerProduct(x: T) = x.value.lastKey

  def combine(x: T, y: T, f: (C, C) => C) = {
    val l = new TreeMap(x.value)
    iterator(y).foreach { r =>
      val (s, a) = r
      val c = f(l.getOrElse(s, ring.zero), a)
      if (c.isZero) l -= s else l += ((s, c))
    }
    apply(l)
  }

  override def map(x: T, f: (Array[N], C) => (Array[N], C)) = {
    val l = zero.value
    iterator(x).foreach { r =>
      val (s, a) = r
      val (m, c) = f(s, a)
      if (c.isZero) () else l += ((m, c))
    }
    apply(l)
  }

  def sort(x: T) = x
}

object TreeMutablePolynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: TreeMutablePolynomial[T, C, N]
    val value: SortedMap[Array[N], C]
  }
}
