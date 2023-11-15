package scas.polynomial

trait PolynomialWithGB[T, C, N] extends PolynomialWithEngine[T, C, N] {
  type P = Pair

  def apply(i: Int, j: Int) = new Pair(i, j)
}
