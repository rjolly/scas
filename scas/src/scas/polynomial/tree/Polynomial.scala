package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial
import Polynomial.Ops

abstract class Polynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] {
  given instance: Polynomial[C, M]
  given Ops[C, M] = new Ops[C, M]
}

object Polynomial {
  class Ops[C, M](using Polynomial[C, M]) extends TreePolynomial.Ops[C, M]
  def apply[C : Ring, M : PowerProduct] = new Polynomial[C, M] {
    given instance: Polynomial[C, M] = this
  }
}
