package scas.polynomial

import scas.structure.commutative.UniqueFactorizationDomain

trait PolynomialOverUFD[T, C, M] extends Polynomial[T, C, M] with UniqueFactorizationDomain[T] {
  given ring: UniqueFactorizationDomain[C]
  extension (x: T) def divideAndRemainder(y: T) = {
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
  extension (x: T) def remainder(ys: Seq[T]): T = {
    val it = x.iterator
    if (it.hasNext) {
      val (s, a) = it.next
      ys.find(_.reduce(s, a)) match {
        case Some(y) => {
          val (t, b) = y.head
          x.subtract(s / t, a / b, y).remainder(ys)
        }
        case None => x
      }
    } else x
  }
  extension (x: T) def reduce(s: M, a: C) = {
    val (t, b) = x.head
    (t | s) && (b | a)
  }
  extension (x: T) def reduce(y: T): T = x.reduce(List(y))
  extension (x: T) override def reduce(m: M, a: C, y: T, b: C) = {
    val gcd = ring.gcd(a, b)
    val (a0, b0) = (a / gcd, b / gcd)
    (x%* b0).subtract(m, a0, y)
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
