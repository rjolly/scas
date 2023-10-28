package scas.residue.impl

import scas.power.impl.PowerProduct
import scas.structure.commutative.impl.Field
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element

trait TreeAlgebraicNumber[C : Field, M : PowerProduct] extends AlgebraicNumber[Element[C, M], C, M] {
  val ring: UnivariatePolynomial[C, M] = new UnivariatePolynomial[C, M]
  export ring.coef2poly
}
