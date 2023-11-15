package scas.polynomial

trait WeylAlgebra[T, C, M] extends SolvablePolynomial[T, C, M] {
  val n = length >> 1
  for (i <- 0 until n; j = i + n) {
    val xi = generator(i)
    val xj = generator(j)
    update(xj, xi, xi * xj + one)
  }
}
