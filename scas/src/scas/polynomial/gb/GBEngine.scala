package scas.polynomial.gb

import scas.polynomial.Polynomial

open class GBEngine[T, C, M](using factory: Polynomial[T, C, M]) extends Engine[T, C, M, Pair[M]] {
  import factory.pp

  def apply(i: Int, j: Int, reduction: Boolean, principal: Int, coprime: Boolean, scm: M) = new Pair(i, j, reduction, principal, coprime, scm)
}
