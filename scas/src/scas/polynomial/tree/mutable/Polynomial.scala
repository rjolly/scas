package scas.polynomial.tree.mutable

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreeMutablePolynomial

class Polynomial[C : Ring, M : PowerProduct] extends scas.polynomial.tree.Polynomial[C, M] with TreeMutablePolynomial[C, M] {
    given instance: Polynomial[C, M] = this
}
