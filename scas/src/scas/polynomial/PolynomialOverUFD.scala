package scas.polynomial

import scala.annotation.tailrec
import scas.structure.commutative.UniqueFactorizationDomain

trait PolynomialOverUFD[T, C, M] extends Polynomial[T, C, M] with UniqueFactorizationDomain[T] {
  given ring: UniqueFactorizationDomain[C]
  extension (x: T) {
    def divideAndRemainder(y: T) = {
      if (y.isZero) throw new ArithmeticException("Polynomial divide by zero")
      else if (x.isZero) (zero, zero)
      else {
        val (s, a) = x.head
        if (y.reduce(s, a)) {
          val (t, b) = y.head
          val c = this(s / t, a / b)
          val (q, r) = (x - c * y).divideAndRemainder(y)
          (c + q, r)
        } else (zero, x)
      }
    }
    @tailrec final def remainder(ys: T*): T = {
      val it = x.iterator
      if (it.hasNext) {
        val (s, a) = it.next
        ys.find(_.reduce(s, a)) match {
          case Some(y) => {
            val (t, b) = y.head
            x.subtract(s / t, a / b, y).remainder(ys*)
          }
          case None => x
        }
      } else x
    }
    def reduce(s: M, a: C) = {
      val (t, b) = x.head
      (t | s) && (b | a)
    }
    def remainder(tail: Boolean, ys: T*): T = {
      if (tail) {
        val xs = x.iterator
        if (xs.hasNext) {
          val (s, a) = xs.next
          if (xs.hasNext) {
            val (s, a) = xs.next
            x.remainder(s, ys*)
          } else x
        } else x
      } else x.remainder(ys*).remainder(true, ys*)
    }
    @tailrec final def remainder(m: M, ys: T*): T = {
      val xs = x.iterator(m)
      if (xs.hasNext) {
        val (s, a) = xs.next
        ys.find(_.reduce(s, a)) match {
          case Some(y) => {
            val (t, b) = y.head
            x.subtract(s / t, a / b, y).remainder(m, ys*)
          }
          case None => {
            if (xs.hasNext) {
              val (s, a) = xs.next
              x.remainder(s, ys*)
            } else x
          }
        }
      } else x
    }
    override def reduce(m: M, a: C, y: T, b: C) = {
      val gcd = ring.gcd(a, b)
      val (a0, b0) = (a / gcd, b / gcd)
      (x%* b0).subtract(m, a0, y)
    }
  }
  def content(x: T) = {
    val c = x.iterator.foldLeft(ring.zero) { (l, r) =>
      val (_, a) = r
      ring.gcd(l, a)
    }
    val d = ring.abs(c)
    if (x.signum < 0) -d else d
  }
  def contentAndPrimitivePart(x: T) = {
    if (x.isZero) (ring.zero, zero) else {
      val c = content(x)
      (c, x%/ c)
    }
  }
  def primitivePart(x: T) = { val (c, p) = contentAndPrimitivePart(x) ; p }
  extension (x: T) def %/ (c: C) = x.map((s, a) => (s, a / c))

  extension (ring: UniqueFactorizationDomain[C]) def apply(s: T*) = {
    same(s*)
    this
  }
}
