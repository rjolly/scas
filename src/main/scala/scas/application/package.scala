package scas

package object application {
  type Poly = MultivariatePolynomial.Element[BigInteger, Int]
  type RF = RationalFunction.Element[Poly, BigInteger, Int]
}
