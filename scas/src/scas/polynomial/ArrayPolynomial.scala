package scas.polynomial

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.compiletime.deferred
import scas.power.offset.PowerProduct
import ArrayPolynomial.Element

trait ArrayPolynomial[C, N] extends Polynomial[Element[C, N], C, Array[N]] {
  given pp: PowerProduct[N] = deferred
  given ClassTag[N] = deferred
  given ClassTag[C] = deferred
  def length = pp.one.length
  def zero(n: Int): Element[C, N] = Element(new Array[N](n * length), new Array[C](n), Array(0))
  def apply(s: (Array[N], C)*) = {
    val r = zero(s.size)
    s.foreach { r.add(_) }
    r
  }

  extension (x: Element[C, N]) {
    def pack = {
      val r = zero(x.size)
      r.add(x, 0)
      r
    }
    def add(r: (Array[N], C)): Unit = {
      val (m, c) = r
      x.add(m, 0, c)
    }
    def add(m: Array[N], n: Int, c: C): Unit = {
      val Element(s, a, size) = x
      a(size(0)) = c
      System.arraycopy(m, n * length, s, size(0) * length, length)
      size(0) += 1
    }
    def add(y: Element[C, N], n: Int): Unit = {
      x.add(y, n, y.size)
    }
    def add(y: Element[C, N], n: Int, m: Int): Unit = {
      val Element(s, a, size) = x
      val Element(t, b, _) = y
      val d = m - n
      System.arraycopy(b, n, a, size(0), d)
      System.arraycopy(t, n * length, s, size(0) * length, d * length)
      size(0) += d
    }
    def add(y: Element[C, N]) = {
      val Element(s, a, Array(x_size)) = x
      val Element(t, b, Array(y_size)) = y
      val r = zero(x_size + y_size)
      var i = 0
      var j = 0
      var i0 = i
      var j0 = j
      while i < x_size && j < y_size do {
        if pp.compare(s, i, t, j) > 0 then {
          if j > j0 then { r.add(y, j0, j); j0 = j }
          i += 1
        } else if pp.compare(s, i, t, j) < 0 then {
          if i > i0 then { r.add(x, i0, i); i0 = i }
          j += 1
        } else {
          if i > i0 then { r.add(x, i0, i); i0 = i }
          if j > j0 then { r.add(y, j0, j); j0 = j }
          val c = a(i) + b(j)
          if !c.isZero then r.add(s, i, c)
          i += 1
          j += 1
          i0 = i
          j0 = j
        }
      }
      if i > i0 then { r.add(x, i0, i); i0 = i }
      if j > j0 then { r.add(y, j0, j); j0 = j }
      r.add(x, i)
      r.add(y, j)
      if r.size < x_size + y_size then r.pack else r
    }

    def iterator = new Iterator(x, 0)

    override def iterator(m: Array[N]) = new Iterator(x, x.indexOf(m))

    def indexOf(m: Array[N]): Int = x.indexOf(m, 0, x.size)

    @tailrec final def indexOf(m: Array[N], i: Int, j: Int): Int = {
      val n = (i + j) >> 1
      if n >= i then -1 else {
        val (s, _) = x(n)
        if s < m then x.indexOf(m, i, n)
        else if s > m then x.indexOf(m, n + 1, j)
        else n
      }
    }

    override def isZero = x.size == 0

    def size = x.sz(0)

    def head = x(0)

    def last = x(x.size - 1)

    def apply(n: Int) = {
      val m = pp.empty
      val Element(s, a, _) = x
      System.arraycopy(s, n * length, m, 0, length)
      (m, a(n))
    }

    override def multiply(m: Array[N], c: C) = {
      val r = zero(x.size)
      val Element(s, a, _) = x
      val Element(t, b, size) = r
      var i = 0
      while i < x.size do {
        val ac = a(i) * c
        assert (!ac.isZero)
        pp.multiply(s, i, m, t)
        b(i) = ac
        size(0) += 1
        i += 1
      }
      r
    }

    def map(f: (Array[N], C) => (Array[N], C)) = {
      val r = zero(x.size)
      var i = 0
      while i < x.size do {
        val (s, a) = x(i)
        val (m, c) = f(s, a)
        assert (!c.isZero)
        r.add((m, c))
        i += 1
      }
      r
    }
  }

  class Iterator(x: Element[C, N], n: Int) extends scala.Iterator[(Array[N], C)] {
    var i = n
    def hasNext = i < x.size
    def next = {
      if i >= x.size then scala.Iterator.empty.next else {
        val (s, a) = x(i)
        i += 1
        (s, a)
      }
    }
  }
}

object ArrayPolynomial {
  case class Element[C, N](m: Array[N], c: Array[C], sz: Array[Int])
}
