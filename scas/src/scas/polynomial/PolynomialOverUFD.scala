package scas.polynomial

import scas.structure.commutative.UniqueFactorizationDomain

trait PolynomialOverUFD[T, C, M] extends Polynomial[T, C, M] with UniqueFactorizationDomain[T] {
  def ring: UniqueFactorizationDomain[C]
  given UniqueFactorizationDomain[C] = ring
  extension (x: T) def divideAndRemainder(y: T) = {
    if (y.isZero) throw new ArithmeticException("Polynomial divide by zero")
    else if (x.isZero) (zero, zero)
    else {
      val (s, a) = head(x)
      List(y).find(_.reduce(s, a)) match {
        case Some(y) => {
          val (t, b) = head(y)
          val c = this(s / t, a / b)
          val (q, r) = (x - c * y).divideAndRemainder(y)
          (c + q, r)
        }
        case None => (zero, x)
      }
    }
  }
  extension (x: T) def remainder(ys: Seq[T]): T = {
    val it = iterator(x)
    if (it.hasNext) {
      val (s, a) = it.next
      ys.find(_.reduce(s, a)) match {
        case Some(y) => {
          val (t, b) = head(y)
          x.subtract(s / t, a / b, y).remainder(ys)
        }
        case None => x
      }
    } else x
  }
  extension (x: T) def reduce(s: M, a: C) = {
    val (t, b) = head(x)
    (t | s) && (b | a)
  }
  extension (x: T) def reduce(y: T): T = x.reduce(List(y))
  extension (x: T) override def reduce(m: M, a: C, y: T, b: C) = {
    val gcd = ring.gcd(a, b)
    val (a0, b0) = (a / gcd, b / gcd)
    (x%* b0).subtract(m, a0, y)
  }
  def content(x: T) = {
    val c = iterator(x).foldLeft(ring.zero) { (l, r) =>
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
}
