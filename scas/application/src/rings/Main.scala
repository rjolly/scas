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

given id[T] as Conversion[T, T] = identity
given int2bigInt as Conversion[Int, BigInteger] = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given long2bigInt as Conversion[Long, BigInteger] = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given coef2poly[U, C : MultivariatePolynomialRing](using Conversion[U, C]) as Conversion[U, MultivariatePolynomial[C]] = MultivariatePolynomialRing[C].ring.factory().createConstant(_)

given bigInt2scas[U](using Conversion[U, BigInteger]) as Conversion[U, scas.BigInteger] = (x: U) => java.math.BigInteger(x.toByteArray)

def (a: Long) \:(b: Long) = BigInteger(a) \ b
