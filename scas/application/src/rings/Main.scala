package rings

import scala.util.FromDigits
import cc.redberry.rings.Rings
import cc.redberry.rings.Integers
import cc.redberry.rings.poly.multivar.MultivariatePolynomial

type BigInteger = cc.redberry.rings.bigint.BigInteger

given BigInteger as Ring[BigInteger] with FromDigits[BigInteger] {
  def fromDigits(digits: String) = new BigInteger(digits)
  val ring: Integers = Rings.Z
}

given int2bigInt as Conversion[Int, BigInteger] = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given long2bigInt as Conversion[Long, BigInteger] = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given coef2poly[U, C : MultivariatePolynomialRing](using Conversion[U, C]) as Conversion[U, MultivariatePolynomial[C]] = (x: U) => (x: C): MultivariatePolynomial[C]
given coef2poly[C : MultivariatePolynomialRing] as Conversion[C, MultivariatePolynomial[C]] = MultivariatePolynomialRing[C].ring.factory().createConstant(_)

given bigInt2scas[U](using Conversion[U, BigInteger]) as Conversion[U, scas.BigInteger] = (x: U) => (x: BigInteger): scas.BigInteger
given bigInt2scas as Conversion[BigInteger, scas.BigInteger] = (x: BigInteger) => java.math.BigInteger(x.toByteArray)

extension (a: Long) def \:(b: Long) = BigInteger(a) \ b
