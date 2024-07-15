package scas.polynomial.repr

import scala.reflect.ClassTag
import scas.polynomial.{PolynomialOverField, PolynomialWithRepr}
import scas.structure.commutative.Field
import scas.power.PowerProduct
import PolynomialWithRepr.Element

class UnivariatePolynomial[T : ClassTag, C, M](using val factory: PolynomialOverField[T, C, M])(dimension: Int) extends PolynomialWithRepr[T, C, M](dimension) with scas.polynomial.UnivariatePolynomial[Element[T], C, M] {
  given ring: Field[C] = factory.ring
  given pp: PowerProduct[M] = factory.pp
}
