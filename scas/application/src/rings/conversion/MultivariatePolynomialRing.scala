package rings.conversion

import cc.redberry.rings.poly.multivar.MultivariatePolynomial
import cc.redberry.rings.poly.multivar.DegreeVector;
import scas.util.{Conversion, unary_~}
import java.util.Comparator

class MultivariatePolynomialRing[C : Ring](monomialOrder: Comparator[DegreeVector], variables: String*) extends rings.MultivariatePolynomialRing[C](monomialOrder, variables: _*) with Ring[MultivariatePolynomial[C]] {
  given MultivariatePolynomialRing[C] = this

  given coef2poly[D: Conversion[C]]: (D => MultivariatePolynomial[C]) = x => ring.factory().createConstant(~x)
}
