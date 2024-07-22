package scas.polynomial

trait PolynomialWithGB[T, C, N] extends PolynomialWithEngine[T, C, N] {
  type P = Pair

  def apply(i: Int, j: Int) = {
    val m = headPowerProduct(i)
    val n = headPowerProduct(j)
    val scm = pp.lcm(m, n)
    new Pair(i, j, m, n, scm)
  }
}
