package scas.polynomial.tree

import scas.structure.impl.Ring
import scas.power.impl.PowerProduct
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element

abstract class Polynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.Polynomial[Element[C, M], C, M] {
  given instance: Polynomial[C, M]
}

object Polynomial {
  def apply[C : Ring, M : PowerProduct]: Polynomial[C, M] = new Polynomial[C, M] {
    given instance: Polynomial[C, M] = this
  }
}
