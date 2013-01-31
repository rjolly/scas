package scas.tree

import scala.reflect.ClassTag
import scas.ufd.SimpleGCD
import scas.polynomial.PowerProduct
import scas.structure.UniqueFactorizationDomain
import MultivariatePolynomial.Element

class PolynomialWithSimpleGCD[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends MultivariatePolynomial[C, N] with SimpleGCD[Element[C, N], C, N] {
  def split = MultivariatePolynomial.withSimpleGCD(MultivariatePolynomial.withSimpleGCD(ring, pp.take(location)), pp.drop(location))
}
