package scas.polynomial

import scas.structure.UniqueFactorizationDomain
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialOverUFD[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] with UniqueFactorizationDomain[T] {
  override implicit val ring: UniqueFactorizationDomain[C]
  def divideAndRemainder(x: T, y: T) = {
    if (y.isZero) throw new ArithmeticException("Polynomial divide by zero")
    else if (x.isZero) (zero, zero)
    else {
      val (s, a) = head(x)
      val (t, b) = head(y)
      if (!(t | s) || !(b | a)) (zero, x) else {
        val c = multiply(one, s / t, a / b)
        val (q, r) = divideAndRemainder(x - c * y, y)
        (c + q, r)
      }
    }
  }
  def remainder(x: T, list: List[T]): T = {
    val it = iterator(x)
    if (it.hasNext) {
      val (s, a) = it.next
      list match {
        case y::tail => {
          val (t, b) = head(y)
          if (!(t | s) || !(b | a)) remainder(x, tail) else {
            remainder(x - multiply(y, s / t, a / b), list)
          }
        }
        case _ => x
      }
    } else x
  }
  override def subtract(x: T, m: Array[N], a: C, y: T, b: C) = {
    val gcd = ring.gcd(a, b)
    val (a0, b0) = (a / gcd, b / gcd)
    multiply(x, b0) - multiply(y, m, a0)
  }
  def content(x: T) = {
    val c = (ring.zero /: iterator(x)) { (l, r) => val (s, a) = r ; ring.gcd(l, a) }
    ring.abs(c) * ring(signum(x))
  }
  def contentAndPrimitivePart(x: T) = {
    if (x.isZero) (ring.zero, zero)
    else {
      val c = content(x)
      (c, divide(x, c))
    }
  }
  def primitivePart(x: T) = { val (c, p) = contentAndPrimitivePart(x) ; p }
  def divide(x: T, c: C) = map(x, (s, a) => (s, a / c))
}

object PolynomialOverUFD {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] with UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: PolynomialOverUFD[T, C, N]
  }
}
