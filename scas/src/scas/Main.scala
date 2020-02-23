package scas

import scas.base.{BigIntegerImpl, RationalImpl}
import scas.structure.Monoid

type BigInteger = java.math.BigInteger
given BigInteger as BigIntegerImpl

given int2bigInt as Conversion[Int, BigInteger] = java.math.BigInteger.valueOf(_)
given long2bigInt as Conversion[Long, BigInteger] = java.math.BigInteger.valueOf(_)

def [T: Monoid](a: T) \: (n: Long): T = a \ n
def (a: Long) \: (n: Long) = (BigInteger(a) \ n).longValue

type Rational = (BigInteger, BigInteger)
given Rational as RationalImpl

given id[T] as Conversion[T, T] = identity
given bigInt2rational[U](using Conversion[U, BigInteger]) as Conversion[U, Rational] = (_, 1)

def (a: Long) /: (b: Long) = Rational(a, b)

def println[T: Show](x: T) = System.out.println(x.toCode)
