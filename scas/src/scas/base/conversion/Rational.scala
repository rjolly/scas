package scas.base.conversion

import scas.structure.commutative.ordered.conversion.Quotient
import scas.util.{Conversion, unary_~}
import scas.base.Rational.Impl
import BigInteger.given

object Rational extends Impl with Quotient[BigInteger] {
  given Rational.type = this
  extension[U: Conversion[BigInteger]] (a: U) {
    def %%[V: Conversion[BigInteger]](b: V) = this(~a, ~b)
  }
  override val zero = Rational("0")
  override val one = Rational("1")
}
