package scas

import scas.power.PowerProduct
import scas.prettyprint.Show

type BigInteger = scas.base.BigInteger
val BigInteger = scas.base.BigInteger
import scas.base.BigInteger.given

given int2bigInt: (Int => BigInteger) = java.math.BigInteger.valueOf(_)
given long2bigInt: (Long => BigInteger) = java.math.BigInteger.valueOf(_)

extension (a: Long) {
  def \ (b: Long) = long2bigInt(a) \ long2bigInt(b)
  def \:(b: Long) = a \ b
}

type Rational = scas.base.Rational
val Rational = scas.base.Rational

given bigInt2rational[U](using c: U => BigInteger): (U => Rational) = x => Rational.fromRing(c(x))

extension (a: Long) def %%(b: Long) = Rational(long2bigInt(a), long2bigInt(b))

def println[T: Show](x: T) = System.out.println(x.show)

type ModInteger = scas.base.ModInteger
val ModInteger = scas.base.ModInteger

given int2powerProduct[M : PowerProduct]: (Int => M) = PowerProduct[M].apply(_)
