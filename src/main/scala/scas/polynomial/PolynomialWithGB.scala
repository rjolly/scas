package scas.polynomial

import scas.gb.Engine
import scas.Implicits.{infixOrderingOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialWithGB[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] with Engine[T, C, N] {
  def normalize(x: T) = primitivePart(x)
  def s_polynomial(x: T, y: T) = {
    val (m, a) = head(x)
    val (n, b) = head(y)
    val gcd = pp.gcd(m, n)
    val (m0, n0) = (m / gcd, n / gcd)
    reduce(x * n0, m0, a, y, b)
  }
  def gcd1(x: T, y: T) = gcd(x, y)
  def gcd(x: T, y: T) = {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    val list = gb(p, q)
    multiply(if (list.size == 1) list(0) else one, ring.gcd(a, b))
  }
}
