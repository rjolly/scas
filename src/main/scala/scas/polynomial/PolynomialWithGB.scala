package scas.polynomial

import scas.polynomial.gb.GBEngine
import PolynomialWithGB.Element

trait PolynomialWithGB[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends PolynomialOverUFD[T, C, N] with PolynomialWithIndex[T, C, N] {
  def gb(xs: T*): List[T] = gb(xs.toList)
  def gb(list: List[T]) = new GBEngine(this).compute(list)
  def gcd(x: T, y: T) = {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    val list = gb(p, q)
    multiply(if (list.size == 1) list(0) else one, ring.gcd(a, b))
  }
}

object PolynomialWithGB {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends PolynomialOverUFD.Element[T, C, N] with PolynomialWithIndex.Element[T, C, N] { this: T =>
    val factory: PolynomialWithGB[T, C, N]
  }
}
