package scas

package object base {
  type BigInteger = java.math.BigInteger
  type Rational = (BigInteger, BigInteger)
  type Complex = Residue.Element[UnivariatePolynomial.Element[Rational, Int], Rational, Int]
}
