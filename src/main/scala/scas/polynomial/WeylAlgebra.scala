package scas.polynomial

import Polynomial.Element

trait WeylAlgebra[T <: Element[T, C, N], C, N] extends SolvablePolynomial[T, C, N] {
  val n = pp.length >> 1
  for (i <- 0 until n; j = i + n) {
    val xi = generator(i)
    val xj = generator(j)
    update(headPowerProduct(xj), headPowerProduct(xi), xi * xj + one)
  }
}
