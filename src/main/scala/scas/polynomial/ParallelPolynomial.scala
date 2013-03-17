package scas.polynomial

import IterablePolynomial.Element

trait ParallelPolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends IterablePolynomial[T, C, N] {
  override def times(x: T, y: T) = y.value.par.aggregate(zero)({ (l, r) =>
    val (a, b) = r
    add(l, a, b, x)
  }, _ + _)
}
