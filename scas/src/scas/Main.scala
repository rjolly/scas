package scas

import scas.base.{BigIntegerImpl, RationalImpl}
import scas.power.PowerProduct
import scas.polynomial.Polynomial
import scas.prettyprint.Show

type BigInteger = java.math.BigInteger
given BigInteger as BigIntegerImpl

given int2bigInt as Conversion[Int, BigInteger] = java.math.BigInteger.valueOf(_)
given long2bigInt as Conversion[Long, BigInteger] = java.math.BigInteger.valueOf(_)

extension (a: Long) def \:(b: Long) = BigInteger(a) \ b

type Rational = (BigInteger, BigInteger)
given Rational as RationalImpl

given id[T] as Conversion[T, T] = identity
given bigInt2rational[U](using Conversion[U, BigInteger]) as Conversion[U, Rational] = (_, 1)

extension (a: Long) def %%(b: Long) = Rational(a, b)

def println[T: Show](x: T) = System.out.println(x.show)

type ModInteger = scas.base.ModInteger

given int2powerProduct[M : PowerProduct] as Conversion[Int, M] = PowerProduct[M].apply(_)
