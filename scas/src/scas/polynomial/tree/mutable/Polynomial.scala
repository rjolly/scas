package scas.polynomial.tree.mutable

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreeMutablePolynomial.Impl
import scas.polynomial.TreePolynomial.Element

class Polynomial[C : Ring.Impl, M](using pp: PowerProduct.Impl[M]) extends Impl[C, M] with scas.polynomial.Polynomial[Element[C, M], C, M] {
  given Polynomial[C, M] = this
}
