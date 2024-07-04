package scas.polynomial.repr

import scala.reflect.ClassTag
import scas.polynomial.{Polynomial, PolynomialWithRepr}
import scas.structure.commutative.Field
import scas.power.PowerProduct
import PolynomialWithRepr.Element

class UnivariatePolynomial[T : ClassTag, C, M](val factory: Polynomial[T, C, M], val ring: Field[C], pp: PowerProduct[M], dimension: Int) extends PolynomialWithRepr[T, C, M](pp, dimension) with scas.polynomial.UnivariatePolynomial[Element[T], C, M]
