package scas.polynomial

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scas.Implicits.{infixOrderingOps, infixRingOps}
import ArrayPolynomial.Element

trait ArrayPolynomial[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] {
  implicit val cm1: ClassTag[C]
  implicit val cm2: ClassTag[N]
  override def isZero(x: T) = x.size == 0
  def zero(n: Int): T = apply((new Array[C](n), pp.one, new Array[N](n * pp.one.length)))
  def apply(value: (Array[C], Array[N], Array[N])): T
  def apply(s: (Array[N], C)*) = {
    val l = zero(s.size)
    s.foreach { l += _ }
    l
  }
  def pack(x: T) = {
    val l = zero(x.size)
    l ++= x
    l
  }

  def plus(x: T, y: T) = {
    val l = zero(x.size + y.size)
    var rx = (pp.one, ring.zero)
    var ry = (pp.one, ring.zero)
    var i = 0
    var j = 0
    var i0 = i
    var j0 = j
    if (i < x.size) rx = x(i)
    if (j < y.size) ry = y(j)
    while (i < x.size && j < y.size) {
      val (s, a) = rx
      val (t, b) = ry
      if (s > t) {
        if (j > j0) { l ++= (y, j0, j); j0 = j }
        i += 1
        if (i < x.size) rx = x(i)
      } else if (s < t) {
        if (i > i0) { l ++= (x, i0, i); i0 = i }
        j += 1
        if (j < y.size) ry = y(j)
      } else {
        if (i > i0) { l ++= (x, i0, i); i0 = i }
        if (j > j0) { l ++= (y, j0, j); j0 = j }
        val c = a + b
        if (!c.isZero) l += ((s, c))
        i += 1
        j += 1
        i0 = i
        j0 = j
        if (i < x.size) rx = x(i)
        if (j < y.size) ry = y(j)
      }
    }
    if (i > i0) { l ++= (x, i0, i); i0 = i }
    if (j > j0) { l ++= (y, j0, j); j0 = j }
    l ++= (x, i)
    l ++= (y, j)
    pack(l)
  }

  def iterator(x: T) = new Iterator(x, 0)

  def iterator(x: T, m: Array[N]) = new Iterator(x, indexOf(x, m))

  class Iterator(x: T, n: Int) extends scala.Iterator[(Array[N], C)] {
    var i = n
    def hasNext = i < x.size
    def next = {
      if (i >= x.size) scala.Iterator.empty.next else {
        val (s, a) = x(i)
        i += 1
        (s.clone, a)
      }
    }
  }

  def indexOf(x: T, m: Array[N]): Int = indexOf(x, m, 0, x.size)

  @tailrec final def indexOf(x: T, m: Array[N], n: Int, k: Int): Int = {
    val i = (n + k) >> 1
    if (i >= k) -1 else {
      val (s, _) = x(i)
      if (s < m) indexOf(x, m, n, i)
      else if (s > m) indexOf(x, m, i + 1, k)
      else i
    }
  }

  def size(x: T) = x.size

  def head(x: T) = x(0)

  def last(x: T) = x(x.size - 1)

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = {
    val l = zero(x.size)
    var i = 0
    while (i < x.size) {
      val (s, a) = x(i)
      val (m, c) = f(s, a)
      if (!c.isZero) l += ((m, c))
      i += 1
    }
    pack(l)
  }

  override def multiply(x: T, m: Array[N], c: C) = {
    val l = zero(x.size)
    var i = 0
    while (i < x.size) {
      val (s, a) = x(i)
      val ac = a * c
      if (!ac.isZero) l += ((pp.multiply(s, m), ac))
      i += 1
    }
    pack(l)
  }

  override def times(x: T, m: Array[N]) = {
    val l = zero(x.size)
    var i = 0
    while (i < x.size) {
      val (s, a) = x(i)
      l += ((pp.multiply(s, m), a))
      i += 1
    }
    l
  }

  override def map(x: T, f: C => C) = {
    val l = x.size
    x.size = 0
    var i = 0
    while (i < l) {
      val (s, a) = x(i)
      val c = f(a)
      if (!c.isZero) x += ((s, c))
      i += 1
    }
    if (l > x.size) pack(x) else x
  }
}

object ArrayPolynomial {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: ArrayPolynomial[T, C, N]
    val value: (Array[C], Array[N], Array[N])
    var size = 0

    def apply(n: Int): (Array[N], C) = {
      System.arraycopy(value._3, n * value._2.length, value._2, 0, value._2.length)
      (value._2, value._1(n))
    }
    def +=(r: (Array[N], C)) = {
      val s = size
      value._1(s) = r._2
      System.arraycopy(r._1, 0, value._3, s * value._2.length, value._2.length)
      size += 1
    }
    def ++=(rest: T): Unit = this ++= (rest, 0)
    def ++=(rest: T, n: Int): Unit = this ++= (rest, n, rest.size)
    def ++=(rest: T, n: Int, m: Int): Unit = {
      val s = m - n
      val ss = size
      System.arraycopy(rest.value._1, n, value._1, ss, s)
      System.arraycopy(rest.value._3, n * value._2.length, value._3, ss * value._2.length, s * value._2.length)
      size += s
    }
  }
}
