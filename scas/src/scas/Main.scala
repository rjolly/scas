package scas

import scas.prettyprint.Show

type BigInteger = scas.base.BigInteger
val BigInteger = scas.base.BigInteger

type Rational = scas.base.Rational
val Rational = scas.base.Rational

given bigInt2rational[U](using c: U => BigInteger): (U => Rational) = x => Rational.fromRing(c(x))

def println[T: Show](x: T) = System.out.println(x.show)

type ModInteger = scas.base.ModInteger
val ModInteger = scas.base.ModInteger
