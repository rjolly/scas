package scas.polynomial

import scas.util.{Conversion, unary_~}
import scas.base.BigInteger

trait UnivariatePolynomial[T, C, M] extends impl.UnivariatePolynomial[T, C, M] with PolynomialOverUFD[T, C, M] {
  extension (x: T) def coefficient[U: Conversion[BigInteger]](n: U) = super.coefficient(x)(~n)
}
