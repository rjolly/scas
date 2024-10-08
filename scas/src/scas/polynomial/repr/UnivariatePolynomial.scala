package scas.polynomial.repr

import scala.reflect.ClassTag
import scas.polynomial.PolynomialWithRepr
import scas.structure.commutative.Field
import scas.power.PowerProduct
import PolynomialWithRepr.Element

class UnivariatePolynomial[T : ClassTag, C, M](using val factory: scas.polynomial.ufd.UnivariatePolynomial[T, C, M])(dimension: Int) extends PolynomialWithRepr[T, C, M](dimension) with scas.polynomial.ufd.UnivariatePolynomial[Element[T], C, M] {
  given ring: Field[C] = factory.ring
  given pp: PowerProduct[M] = factory.pp
}
