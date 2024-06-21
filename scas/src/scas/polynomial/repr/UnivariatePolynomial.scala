package scas.polynomial.repr

import scala.reflect.ClassTag
import scas.polynomial.{Polynomial, PolynomialWithRepr}
import scas.structure.commutative.Field
import scas.power.PowerProduct
import PolynomialWithRepr.Element

class UnivariatePolynomial[T : ClassTag, C : Field, M : PowerProduct](using Polynomial[T, C, M])(val dimension: Int) extends PolynomialWithRepr[T, C, M] with scas.polynomial.UnivariatePolynomial[Element[T], C, M]
