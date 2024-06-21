package scas.polynomial.tree

import scas.structure.Ring
import scas.power.PowerProduct
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class Polynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.structure.conversion.Ring[Element[C, M]] {
  given ring: Ring[C] = summon
  given pp: PowerProduct[M] = summon
  given instance: Polynomial[C, M] = this
}
