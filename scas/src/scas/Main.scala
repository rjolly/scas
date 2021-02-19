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

extension (a: Long) {
  def \ (b: Long) = long2bigInt(a) \ long2bigInt(b)
  def \:(b: Long) = a \ b
}

type Rational = (BigInteger, BigInteger)
object Rational extends RationalImpl {
  given RationalImpl = this
}
import Rational.given

given bigInt2rational[U](using c: Conversion[U, BigInteger]): Conversion[U, Rational] = x => (c(x), BigInteger(1))

extension (a: Long) def %%(b: Long) = Rational(long2bigInt(a), long2bigInt(b))

def println[T: Show](x: T) = System.out.println(x.show)

type ModInteger = scas.base.ModInteger
val ModInteger = scas.base.ModInteger

given int2powerProduct[M : PowerProduct]: Conversion[Int, M] = PowerProduct[M].apply(_)
