package scas.polynomial

import Polynomial.Element

trait WeylAlgebra[T <: Element[T, C, N], C, N] extends SolvablePolynomial[T, C, N] {
  val n = variables.length >> 1
  for (i <- 0 until n; j = i + n) {
    val xi = generator(i)
    val xj = generator(j)
    update(xj, xi, xi * xj + one)
  }
}
