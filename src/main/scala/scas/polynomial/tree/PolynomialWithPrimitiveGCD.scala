package scas.polynomial.tree

import scas.polynomial.PowerProduct
import scas.structure.UniqueFactorizationDomain
import MultivariatePolynomial.Element

class PolynomialWithPrimitiveGCD[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends MultivariatePolynomial[C, N] with scas.polynomial.PolynomialWithPrimitiveGCD[Element, C, N] {
  def split = MultivariatePolynomial.withPrimitiveGCD(MultivariatePolynomial.withPrimitiveGCD(ring, pp.take(location)), pp.drop(location))
}
