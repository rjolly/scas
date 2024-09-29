package scas.adapter.rings.conversion

import cc.redberry.rings.poly.multivar.MultivariatePolynomial
import cc.redberry.rings.poly.multivar.DegreeVector;
import java.util.Comparator

class MultivariatePolynomialRing[C : scas.adapter.rings.Ring](monomialOrder: Comparator[DegreeVector], variables: String*) extends scas.adapter.rings.MultivariatePolynomialRing[C](monomialOrder, variables*) with Ring[MultivariatePolynomial[C]] {
  given instance: MultivariatePolynomialRing[C] = this
}
