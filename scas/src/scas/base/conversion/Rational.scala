package scas.base.conversion

import scas.structure.commutative.ordered.conversion.Quotient
import scas.base.Rational.Impl
import BigInteger.given

object Rational extends Impl with Quotient[BigInteger] {
  given Rational.type = this
  extension (a: Long) def %%(b: Long) = this(long2bigInt(a), long2bigInt(b))
}
