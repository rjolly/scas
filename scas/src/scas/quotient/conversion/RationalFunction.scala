package scas.quotient.conversion

import scas.polynomial.PolynomialOverUFD
import scas.util.{Conversion, unary_~}
import scas.base.BigInteger

class RationalFunction[T, M](using val ring: PolynomialOverUFD[T, BigInteger, M]) extends scas.quotient.RationalFunction[T, M] with Quotient[T, BigInteger, M] {
  given instance: RationalFunction[T, M] = this
  extension[U: Conversion[BigInteger]] (a: U) {
    def %%[V: Conversion[BigInteger]](b: V) = this(ring(~a), ring(~b))
  }
}
