package scas.polynomial.tree.mutable

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreeMutablePolynomial

class Polynomial[C : Ring, M : PowerProduct] extends TreeMutablePolynomial[C, M] {
    given instance: Polynomial[C, M] = this
}