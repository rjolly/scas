package scas.polynomial.repr

import scala.reflect.ClassTag
import scas.polynomial.PolynomialWithRepr
import scas.structure.commutative.Field
import scas.power.PowerProduct
import PolynomialWithRepr.Element

class UnivariatePolynomial[T, C, M](using val factory: scas.polynomial.ufd.UnivariatePolynomial[T, C, M])(val dimension: Int)(using val cm: ClassTag[T]) extends PolynomialWithRepr[T, C, M] with scas.polynomial.ufd.UnivariatePolynomial[Element[T], C, M] {
  given ring: Field[C] = factory.ring
  given pp: PowerProduct[M] = factory.pp
}
