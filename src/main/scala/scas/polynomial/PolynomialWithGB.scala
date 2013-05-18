package scas.polynomial

import scas.ufd.GBGCD
import scas.Implicits.{infixOrderingOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialWithGB[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] with GBGCD[T, C, N] {
  def gb(xs: T*): Seq[T]
  def normalize(x: T) = primitivePart(x)
  def s_polynomial(x: T, y: T) = {
    val (m, a) = head(x)
    val (n, b) = head(y)
    val gcd = pp.gcd(m, n)
    val (m0, n0) = (m / gcd, n / gcd)
    reduce(x * n0, m0, a, y, b)
  }
}
