package scas.polynomial

class GBEngine[T, C, N](using factory: PolynomialWithGB[T, C, N]) extends Engine[T, C, N, Pair] {
  import factory.pp

  def apply(i: Int, j: Int) = {
    val m = i.headPowerProduct
    val n = j.headPowerProduct
    val scm = pp.lcm(m, n)
    new Pair(i, j, m, n, scm)
  }
}
