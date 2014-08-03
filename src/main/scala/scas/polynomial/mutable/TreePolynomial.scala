package scas.polynomial.mutable

import java.util.{SortedMap, TreeMap}
import scala.collection.convert.{WrapAsScala, WrapAsJava}
import scas.polynomial.Polynomial
import scas.Implicits.{infixRingOps, infixPowerProductOps}
import TreePolynomial.Element
import WrapAsScala.{mapAsScalaMap, asScalaIterator}
import WrapAsJava.asJavaIterator

trait TreePolynomial[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] {
  override def isZero(x: T) = x.value.isEmpty
  def clone(x: T) = apply(new TreeMap(x.value))
  def apply(s: (Array[N], C)*) = {
    val l = new TreeMap[Array[N], C](pp.reverse)
    s.foreach { l += _ }
    apply(l)
  }
  def apply(value: SortedMap[Array[N], C]): T

  def plus(x: T, y: T) = add(clone(x), y)

  def iterator(x: T) = x.value.iterator

  override def iterator(x: T, m: Array[N]) = x.value.tailMap(m).iterator

  override def toSeq(x: T) = x.value.toSeq

  def size(x: T) = x.value.size

  def head(x: T) = { val a = headPowerProduct(x) ; (a, x.value.get(a)) }

  override def headPowerProduct(x: T) = x.value.firstKey

  def last(x: T) = { val a = lastPowerProduct(x) ; (a, x.value.get(a)) }

  def lastPowerProduct(x: T) = x.value.lastKey

  override def coefficient(x: T, m: Array[N]) = x.value.getOrElse(m, ring.zero)

  def add(x: T, y: T) = {
    val l = x.value
    y.value.foreach { r =>
      val (s, a) = r
      val c = l.getOrElse(s, ring.zero) + a
      if (c.isZero) l -= s else l += ((s, c))
    }
    x
  }

  override def subtract(x: T, m: Array[N], c: C, y: T) = {
    val l = x.value
    y.value.foreach { r =>
      val (s, a) = r
      val (sm, ac) = (s * m, a * c)
      val cc = l.getOrElse(sm, ring.zero) - ac
      if (cc.isZero) l -= sm else l += ((sm, cc))
    }
    x
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

  override def map(x: T, f: C => C) = {
    val l = x.value
    val it = asJavaIterator(l.iterator)
    it.foreach { r =>
      val (s, a) = r
      val c = f(a)
      if (c.isZero) it.remove else l += ((s, c))
    }
    x
  }

  override def sort(x: T) = x
}

object TreePolynomial {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: TreePolynomial[T, C, N]
    val value: SortedMap[Array[N], C]
  }
}
