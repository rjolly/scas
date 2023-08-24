package scas.polynomial.tree.mutable

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreeMutablePolynomial

class Polynomial[C : Ring.Impl, M : PowerProduct.Impl] extends TreeMutablePolynomial[C, M] {
  given Polynomial[C, M] = this
}
