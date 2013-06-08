package scas

package object base {
  type BigInteger = java.math.BigInteger
  type Rational = (BigInteger, BigInteger)

  object BigInteger extends BigIntegerLike
  object Rational extends RationalLike
}
