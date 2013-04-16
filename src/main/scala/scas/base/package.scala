package scas

package object base {
  type BigInteger = java.math.BigInteger
  type Rational = (BigInteger, BigInteger)
  type Complex = Residue.Element[UnivariatePolynomial.Element[Rational, Int], Rational, Int]

  object BigInteger extends BigIntegerLike
  object Rational extends RationalLike
  object Complex extends ComplexLike
}
