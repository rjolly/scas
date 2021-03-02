package rings

import scala.util.FromDigits
import cc.redberry.rings.Rings
import cc.redberry.rings.Integers
import cc.redberry.rings.poly.multivar.MultivariatePolynomial

type BigInteger = cc.redberry.rings.bigint.BigInteger

object BigInteger extends Ring[BigInteger] with FromDigits[BigInteger] {
  given this.type = this
  def fromDigits(digits: String) = new BigInteger(digits)
  val ring: Integers = Rings.Z
}
import BigInteger.given

given int2bigInt: (Int => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given long2bigInt: (Long => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given coef2poly[U, C : MultivariatePolynomialRing](using c: U => C): (U => MultivariatePolynomial[C]) = x => MultivariatePolynomialRing[C].ring.factory().createConstant(c(x))

given bigInt2scas[U](using c: U => BigInteger): (U => scas.BigInteger) = x => java.math.BigInteger(c(x).toByteArray)

extension (a: Long) def \:(b: Long) = long2bigInt(a) \ bigInt2scas.apply(long2bigInt(b))
