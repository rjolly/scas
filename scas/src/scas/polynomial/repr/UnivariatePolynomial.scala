package scas.polynomial.repr

import scala.reflect.ClassTag
import scas.polynomial.PolynomialWithRepr.Element

class UnivariatePolynomial[T : ClassTag, C, M](using scas.polynomial.ufd.UnivariatePolynomial[T, C, M])(val dimension: Int) extends PolynomialOverField[T, C, M] with scas.polynomial.ufd.UnivariatePolynomial[Element[T], C, M]
