package scas.ufd

import scas.polynomial.PolynomialWithGB
import PolynomialWithGB.Element

trait GBGCD[T <: Element[T, C, N], C, N] { this: PolynomialWithGB[T, C, N] =>
  def gcd1(x: T, y: T) = gcd(x, y)
  def gcd(x: T, y: T) = {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    val list = gb(p, q)
    multiply(if (list.size == 1) list(0) else one, ring.gcd(a, b))
  }
}
