package scas.polynomial.tree.ufd

import scala.reflect.ClassTag
import scas.power.PowerProduct
import scas.structure.UniqueFactorizationDomain
import MultivariatePolynomial.Element

class PolynomialWithSimpleGCD[C, N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends MultivariatePolynomial[C, N] with scas.polynomial.ufd.PolynomialWithSimpleGCD[Element[C, N], C, N] {
  def split = MultivariatePolynomial.withSimpleGCD(MultivariatePolynomial.withSimpleGCD(ring, pp.take(location)), pp.drop(location))
}
