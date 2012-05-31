package scas.polynomial.tree

import scas.polynomial.PowerProduct
import scas.structure.UniqueFactorizationDomain
import MultivariatePolynomial.Element

class PolynomialWithSimpleGCD[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends MultivariatePolynomial[C, N] with scas.polynomial.PolynomialWithSimpleGCD[Element, C, N] {
  def split = MultivariatePolynomial.withSimpleGCD(MultivariatePolynomial.withSimpleGCD(ring, pp.take(location)), pp.drop(location))
}
