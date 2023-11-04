package scas.quotient.conversion

import scas.polynomial.PolynomialOverUFD
import scas.util.{Conversion, unary_~}
import scas.base.BigInteger

class RationalFunctionOverInteger[T, M](using ring: PolynomialOverUFD[T, BigInteger, M]) extends scas.quotient.RationalFunctionOverInteger[T, M] with Quotient[T, BigInteger, M] {
  given instance: RationalFunctionOverInteger[T, M] = this
  extension[U: Conversion[BigInteger]] (a: U) {
    def %%[V: Conversion[BigInteger]](b: V) = this(ring(~a), ring(~b))
  }
}
