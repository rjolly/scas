package scas.polynomial.tree.mutable

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreeMutablePolynomial
import Polynomial.Ops

class Polynomial[C : Ring, M : PowerProduct] extends TreeMutablePolynomial[C, M] {
  given Polynomial[C, M] = this
  given Ops[C, M] = new Ops[C, M]
}

object Polynomial {
  class Ops[C, M](using Polynomial[C, M]) extends TreeMutablePolynomial.Ops[C, M]
}
