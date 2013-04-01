package scas.polynomial

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scas.Implicits.{infixOrderingOps, infixRingOps}
import ArrayPolynomial.Element

trait ArrayPolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  implicit val cm1: ClassTag[C]
  implicit val cm2: ClassTag[N]
  override def isZero(x: T) = x.size == 0
  def zero(n: Int): T = apply((new Array[C](n), new Array[N](n * pp.one.length)))
  def apply(value: (Array[C], Array[N])): T
  def apply(s: (Array[N], C)*) = {
    val l = zero(s.size)
    s.foreach { l += _ }
    l
  }
  def pack(x: T) = {
    val l = zero(x.size)
    x.dump(l)
    l
  }

  def plus(x: T, y: T) = {
    val q = pp.one
    val r = pp.one
    val l = zero(x.size + y.size)
    var i = 0
    var j = 0
    while (i < x.size && j < y.size) {
      val (s, a) = x(i, q)
      val (t, b) = y(j, r)
      if (s > t) {
        l += ((s, a))
        i += 1
      } else if (s < t) {
        l += ((t, b))
        j += 1
      } else {
        val c = a + b
        if (!c.isZero) l += ((s, c))
        i += 1
        j += 1
      }
    }
    while (i < x.size) {
      val (s, a) = x(i, q)
      l += ((s, a))
      i += 1
    }
    while (j < y.size) {
      val (t, b) = y(j, r)
      l += ((t, b))
      j += 1
    }
    pack(l)
  }

  def iterator(x: T) = new Iterator(x, 0)

  def iterator(x: T, m: Array[N]) = new Iterator(x, indexOf(x, m))

  class Iterator(x: T, n: Int) extends scala.Iterator[(Array[N], C)] {
    var i = n
    def hasNext = i < x.size
    def next = {
      if (i >= x.size) scala.Iterator.empty.next else {
        val result = x(i, pp.one)
        i += 1
        result
      }
    }
  }

  def indexOf(x: T, m: Array[N]): Int = indexOf(x, m, 0, x.size)

  @tailrec final def indexOf(x: T, m: Array[N], n: Int, k: Int): Int = {
    val i = (n + k) >> 1
    if (i >= k) -1 else {
      val (s, _) = x(i, pp.one)
      if (s < m) indexOf(x, m, n, i)
      else if (s > m) indexOf(x, m, i + 1, k)
      else i
    }
  }

  def size(x: T) = x.size

  def head(x: T) = x(0, pp.one)

  def last(x: T) = x(x.size - 1, pp.one)

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = {
    val r = pp.one
    val l = zero(x.size)
    var i = 0
    while (i < x.size) {
      val (s, a) = x(i, r)
      val (m, c) = f(s, a)
      if (!c.isZero) l += ((m, c))
      i += 1
    }
    pack(l)
  }

  override def map(x: T, f: C => C) = {
    val r = pp.one
    val l = x.size
    x.size = 0
    var i = 0
    while (i < l) {
      val (s, a) = x(i, r)
      val c = f(a)
      if (!c.isZero) x += ((s, c))
      i += 1
    }
    if (l > x.size) pack(x) else x
  }
}

object ArrayPolynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: ArrayPolynomial[T, C, N]
    val value: (Array[C], Array[N])
    var size = 0

    def apply(n: Int, m: Array[N]) = {
      System.arraycopy(value._2, n * m.length, m, 0, m.length)
      (m, value._1(n))
    }
    def update(n: Int, r: (Array[N], C)) = {
      val (m, c) = r
      value._1(n) = c
      System.arraycopy(m, 0, value._2, n * m.length, m.length)
    }
    def +=(r: (Array[N], C)) = {
      update(size, r)
      size += 1
    }
    def dump(that: T) = {
      System.arraycopy(value._1, 0, that.value._1, 0, that.value._1.length)
      System.arraycopy(value._2, 0, that.value._2, 0, that.value._2.length)
      that.size = size
    }
  }
}
