package scas

import scas.base.{BigIntegerImpl, RationalImpl}
import scas.power.PowerProduct
import scas.polynomial.Polynomial
import scas.prettyprint.Show

type BigInteger = java.math.BigInteger
object BigInteger extends BigIntegerImpl {
  given BigIntegerImpl = this
}
import BigInteger.given

given int2bigInt: Conversion[Int, BigInteger] = java.math.BigInteger.valueOf(_)
given long2bigInt: Conversion[Long, BigInteger] = java.math.BigInteger.valueOf(_)

extension (a: Long) def \:(b: Long) = BigInteger(a) \ b

type Rational = (BigInteger, BigInteger)
object Rational extends RationalImpl {
  given RationalImpl = this
}
import Rational.given

given bigInt2rational[U](using Conversion[U, BigInteger]): Conversion[U, Rational] = (_, 1)

extension (a: Long) def %%(b: Long) = Rational(a, b)

def println[T: Show](x: T) = System.out.println(x.show)

type ModInteger = scas.base.ModInteger

given int2powerProduct[M : PowerProduct.Factory]: Conversion[Int, M] = PowerProduct[M].apply(_)
