package scas.polynomial

import Polynomial.Element

trait WeylAlgebra[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends SolvablePolynomial[T, C, N] {
  val n = pp.length >> 1
  for (i <- 0 until n; j = i + n) {
    val xi = generators(i)
    val xj = generators(j)
    update(xj, xi, xi * xj + one)
  }
}
