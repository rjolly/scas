package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial

abstract class Polynomial[C : Ring.Impl, M : PowerProduct.Impl] extends TreePolynomial[C, M] {
  given instance: Polynomial[C, M]
}

object Polynomial {
  def apply[C : Ring.Impl, M : PowerProduct.Impl] = new Polynomial[C, M] {
    given instance: Polynomial[C, M] = this
  }
}
