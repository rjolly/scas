package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial

abstract class Polynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with TreePolynomial.Ops[C, M] {
  given instance: Polynomial[C, M]
}

object Polynomial {
  def apply[C : Ring, M : PowerProduct] = new Polynomial[C, M] {
    given instance: Polynomial[C, M] = this
  }
}
