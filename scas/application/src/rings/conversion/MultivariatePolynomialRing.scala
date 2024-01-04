package rings.conversion

import cc.redberry.rings.poly.multivar.MultivariatePolynomial
import cc.redberry.rings.poly.multivar.DegreeVector;
import java.util.Comparator

class MultivariatePolynomialRing[C : rings.Ring](monomialOrder: Comparator[DegreeVector], variables: String*) extends rings.MultivariatePolynomialRing[C](monomialOrder, variables*) with Ring[MultivariatePolynomial[C]] {
  given instance: MultivariatePolynomialRing[C] = this
}
