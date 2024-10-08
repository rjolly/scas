package scas.polynomial.gb

import scas.polynomial.Polynomial

class GBEngine[T, C, M](using factory: Polynomial[T, C, M]) extends Engine[T, C, M, Pair] {
  import factory.pp

  def apply(i: Int, j: Int) = {
    val m = i.headPowerProduct
    val n = j.headPowerProduct
    val scm = pp.lcm(m, n)
    new Pair(i, j, m, n, scm)
  }
}
