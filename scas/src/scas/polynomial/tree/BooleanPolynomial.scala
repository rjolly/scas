package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.conversion.BooleanRing
import scas.structure.commutative.conversion.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element
import scas.base.Boolean

class BooleanPolynomial[M](using val pp: PowerProduct[M]) extends TreePolynomial[Boolean, M] with scas.polynomial.BooleanPolynomial[Element[Boolean, M], M] with UniqueFactorizationDomain[Element[Boolean, M]] with BooleanRing[Element[Boolean, M]] {
  given instance: BooleanPolynomial[M] = this
}
