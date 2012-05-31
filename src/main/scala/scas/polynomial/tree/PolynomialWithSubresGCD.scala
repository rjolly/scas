package scas.polynomial.tree

import scas.polynomial.PowerProduct
import scas.structure.UniqueFactorizationDomain
import MultivariatePolynomial.Element

class PolynomialWithSubresGCD[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends MultivariatePolynomial[C, N] with scas.polynomial.PolynomialWithSubresGCD[Element, C, N] {
  def split = MultivariatePolynomial(MultivariatePolynomial(ring, pp.take(location)), pp.drop(location))
}
