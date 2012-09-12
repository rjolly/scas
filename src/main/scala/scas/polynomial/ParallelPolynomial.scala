package scas.polynomial

import Polynomial.Element

trait ParallelPolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  override def times(x: T, y: T) = toSeq(y).par.aggregate(zero)({ (l, r) =>
    val (a, b) = r
    l + multiply(x, a, b)
  }, _ + _)
}
