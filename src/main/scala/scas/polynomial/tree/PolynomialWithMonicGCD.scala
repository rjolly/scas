package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.polynomial.PowerProduct
import scas.structure.Field
import MultivariatePolynomial.Element

class PolynomialWithMonicGCD[C, @specialized(Int, Long) N](override val ring: Field[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends MultivariatePolynomial[C, N] with scas.polynomial.PolynomialOverField[Element[C, N], C, N] {
  def split = MultivariatePolynomial(MultivariatePolynomial(ring, pp.take(location)), pp.drop(location))
}
