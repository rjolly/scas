package scas.polynomial

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scas.power.offset.PowerProduct
import scas.Implicits.{infixOrderingOps, infixRingOps}
import ArrayPolynomial.Element

trait ArrayPolynomial[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] { self =>
  implicit val pp: PowerProduct[N]
  val length = pp.one.length
  implicit val cm1: ClassTag[C]
  implicit val cm2: ClassTag[N]
  override def isZero(x: T) = x.size == 0
  def zero(n: Int): T = apply((new Array[C](n), new Array[N](n * length)))
  def unapply(x: T) = {
    val (c, m) = x.value
    Some(m, c)
  }
  def apply(value: (Array[C], Array[N])): T
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
    val r = zero(x.size + y.size)
    val self(s, a) = x
    val self(t, b) = y
    var i = 0
    var j = 0
    var k = 0
    var l = 0
    var i0 = i
    var j0 = j
    while (i < x.size && j < y.size) {
      if (pp.compare(s, k, t, l) > 0) {
        if (j > j0) { r ++= (y, j0, j); j0 = j }
        i += 1
        k += length
      } else if (pp.compare(s, k, t, l) < 0) {
        if (i > i0) { r ++= (x, i0, i); i0 = i }
        j += 1
        l += length
      } else {
        if (i > i0) { r ++= (x, i0, i); i0 = i }
        if (j > j0) { r ++= (y, j0, j); j0 = j }
        val c = a(i) + b(j)
        if (!c.isZero) r += (s, k, c)
        i += 1
        j += 1
        k += length
        l += length
        i0 = i
        j0 = j
      }
    }
    if (i > i0) { r ++= (x, i0, i); i0 = i }
    if (j > j0) { r ++= (y, j0, j); j0 = j }
    r ++= (x, i)
    r ++= (y, j)
    pack(r)
  }

  def iterator(x: T) = new Iterator(x, 0)

  override def iterator(x: T, m: Array[N]) = new Iterator(x, indexOf(x, m))

  class Iterator(x: T, n: Int) extends scala.Iterator[(Array[N], C)] {
    var i = n
    def hasNext = i < x.size
    def next = {
      if (i >= x.size) scala.Iterator.empty.next else {
        val (s, a) = x(i)
        i += 1
        (s, a)
      }
    }
  }

  def indexOf(x: T, m: Array[N]): Int = indexOf(x, m, 0, x.size)

  @tailrec final def indexOf(x: T, m: Array[N], i: Int, j: Int): Int = {
    val n = (i + j) >> 1
    if (n >= i) -1 else {
      val (s, _) = x(n)
      if (s < m) indexOf(x, m, i, n)
      else if (s > m) indexOf(x, m, n + 1, j)
      else n
    }
  }

  def size(x: T) = x.size

  def head(x: T) = x(0)

  def last(x: T) = x(x.size - 1)

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = {
    val r = zero(x.size)
    var i = 0
    while (i < x.size) {
      val (s, a) = x(i)
      val (m, c) = f(s, a)
      if (!c.isZero) r += ((m, c))
      i += 1
    }
    pack(r)
  }

  override def multiply(x: T, m: Array[N], c: C) = {
    val r = zero(x.size)
    val self(s, a) = x
    var i = 0
    var k = 0
    while (i < x.size) {
      val ac = a(i) * c
      if (!ac.isZero) r += ((pp.times(s, k, m, 0), ac))
      i += 1
      k += length
    }
    pack(r)
  }

  override def times(x: T, m: Array[N]) = {
    val r = zero(x.size)
    val self(s, a) = x
    var i = 0
    var k = 0
    while (i < x.size) {
      r += ((pp.times(s, k, m, 0), a(i)))
      i += 1
      k += length
    }
    r
  }

  override def map(x: T, f: C => C) = {
    val r = x.size
    x.size = 0
    var i = 0
    while (i < r) {
      val (s, a) = x(i)
      val c = f(a)
      if (!c.isZero) x += ((s, c))
      i += 1
    }
    if (r > x.size) pack(x) else x
  }
}

object ArrayPolynomial {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: ArrayPolynomial[T, C, N]
    val value: (Array[C], Array[N])
    import factory.length
    import factory.pp.one
    var size = 0

    def apply(n: Int) = {
      val m = one
      System.arraycopy(value._2, n * length, m, 0, length)
      (m, value._1(n))
    }
    def +=(r: (Array[N], C)): Unit = this += (r._1, 0, r._2)
    def +=(m: Array[N], k: Int, c: C): Unit = {
      val s = size
      value._1(s) = c
      System.arraycopy(m, k, value._2, s * length, length)
      size += 1
    }
    def ++=(rest: T): Unit = this ++= (rest, 0)
    def ++=(rest: T, n: Int): Unit = this ++= (rest, n, rest.size)
    def ++=(rest: T, n: Int, m: Int): Unit = {
      val s = m - n
      val ss = size
      System.arraycopy(rest.value._1, n, value._1, ss, s)
      System.arraycopy(rest.value._2, n * length, value._2, ss * length, s * length)
      size += s
    }
  }
}
