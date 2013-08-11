package scas

package object base {
  type BigInteger = java.math.BigInteger
  type Rational = (BigInteger, BigInteger)
  type Complex = (Double, Double)

  object BigInteger extends BigIntegerLike
  object Rational extends RationalLike
  object Complex extends ComplexLike
}
