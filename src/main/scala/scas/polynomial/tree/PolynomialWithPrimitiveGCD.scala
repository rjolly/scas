package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.ufd.PrimitiveGCD
import scas.power.PowerProduct
import scas.structure.UniqueFactorizationDomain
import MultivariatePolynomial.Element

class PolynomialWithPrimitiveGCD[C, N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends MultivariatePolynomial[C, N] with PrimitiveGCD[Element[C, N], C, N] {
  def split = MultivariatePolynomial.withPrimitiveGCD(MultivariatePolynomial.withPrimitiveGCD(ring, pp.take(location)), pp.drop(location))
}
