package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial.{Impl, Element}

abstract class Polynomial[C : Ring.Impl, M : PowerProduct.Impl] extends Impl[C, M] with scas.polynomial.Polynomial[Element[C, M], C, M] {
  given instance: Polynomial[C, M]
}

object Polynomial {
  def apply[C : Ring.Impl, M : PowerProduct.Impl]: Polynomial[C, M] = new Polynomial[C, M] {
    given instance: Polynomial[C, M] = this
  }
}
