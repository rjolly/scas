package scas.polynomial.tree.parallel

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.{TreePolynomial, ParallelPolynomial}
import TreePolynomial.Element

class Polynomial[C, M](using val ring: Ring[C], val pp: PowerProduct[M]) extends TreePolynomial[C, M] with ParallelPolynomial[Element[C, M], C, M] with Ring.Conv[Element[C, M]] {
  given instance: Polynomial[C, M] = this
}
