package scas.polynomial

import scas.structure.UniqueFactorizationDomain
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialOverUFD[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] with UniqueFactorizationDomain[T] {
  implicit val ring: UniqueFactorizationDomain[C]
  def gcd1(x: T, y: T): T
  def divideAndRemainder(x: T, y: T) = {
    if (y.isZero) throw new ArithmeticException("Polynomial divide by zero")
    else if (x.isZero) (zero, zero)
    else {
      val (s, a) = head(x)
      List(y).find(reducer(s, a)) match {
        case Some(y) => {
          val (t, b) = head(y)
          val c = multiply(one, s / t, a / b)
          val (q, r) = divideAndRemainder(x - c * y, y)
          (c + q, r)
        }
        case None => (zero, x)
      }
    }
  }
  def remainder(x: T, ys: TraversableOnce[T]): T = {
    val it = iterator(x)
    if (it.hasNext) {
      val (s, a) = it.next
      ys.find(reducer(s, a)) match {
        case Some(y) => {
          val (t, b) = head(y)
          remainder(x - multiply(y, s / t, a / b), ys)
        }
        case None => x
      }
    } else x
  }
  def reducer(s: Array[N], a: C)(y: T) = {
    val (t, b) = head(y)
    (t | s) && (b | a)
  }
  def reduce(x: T, y: T): T = reduce(x, List(y))
  override def reduce(x: T, m: Array[N], a: C, y: T, b: C) = {
    val gcd = ring.gcd(a, b)
    val (a0, b0) = (a / gcd, b / gcd)
    subtract(multiply(x, b0), m, a0, y)
  }
  def content(x: T) = {
    val c = (ring.zero /: iterator(x)) { (l, r) =>
      val (_, a) = r
      ring.gcd(l, a)
    }
    ring.abs(c) * ring(signum(x))
  }
  def contentAndPrimitivePart(x: T) = {
    if (x.isZero) (ring.zero, zero) else {
      val c = content(x)
      (c, divide(x, c))
    }
  }
  def primitivePart(x: T) = { val (c, p) = contentAndPrimitivePart(x) ; p }
  def divide(x: T, c: C) = map(x, a => a / c)
}

object PolynomialOverUFD {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] with UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: PolynomialOverUFD[T, C, N]
  }
}
