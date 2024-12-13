package scas.polynomial.tree.parallel.mutable

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.{TreeMutablePolynomial, ParallelMutablePolynomial}
import scas.polynomial.TreePolynomial.Element

class Polynomial[C : Ring, M : PowerProduct] extends TreeMutablePolynomial[C, M] with ParallelMutablePolynomial[Element[C, M], C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}
