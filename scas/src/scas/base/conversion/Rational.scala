package scas.base.conversion

import scas.structure.commutative.ordered.conversion.Quotient
import scas.base.Rational.Impl
import BigInteger.given

object Rational extends Impl with Quotient[BigInteger] {
  given Rational.type = this
  extension (a: Long) def %%(b: Long) = this(BigInteger.convert(a), BigInteger.convert(b))
  override val zero = Rational("0")
  override val one = Rational("1")
}
