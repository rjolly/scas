package scas

import scas.base.{BigIntegerImpl, RationalImpl}
import scas.power.PowerProduct
import scas.prettyprint.Show

type BigInteger = scas.base.BigInteger
object BigInteger extends BigIntegerImpl {
  given BigInteger.type = this
}
import BigInteger.given

given int2bigInt: (Int => BigInteger) = java.math.BigInteger.valueOf(_)
given long2bigInt: (Long => BigInteger) = java.math.BigInteger.valueOf(_)

extension (a: Long) {
  def \ (b: Long) = long2bigInt(a) \ long2bigInt(b)
  def \:(b: Long) = a \ b
}

type Rational = scas.base.Rational
object Rational extends RationalImpl {
  given Rational.type = this
}

given bigInt2rational[U](using c: U => BigInteger): (U => Rational) = x => Rational.fromRing(c(x))

extension (a: Long) def %%(b: Long) = Rational(long2bigInt(a), long2bigInt(b))

def println[T: Show](x: T) = System.out.println(x.show)

type ModInteger = scas.base.ModInteger
val ModInteger = scas.base.ModInteger

given int2powerProduct[M : PowerProduct]: (Int => M) = PowerProduct[M].apply(_)
