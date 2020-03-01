package scas

import scas.base.{BigIntegerImpl, RationalImpl}
import scas.structure.{AbelianGroup, SemiGroup, Ring}
import scas.polynomial.Polynomial
import scas.prettyprint.Show

type BigInteger = java.math.BigInteger
given BigInteger as BigIntegerImpl

given int2bigInt as Conversion[Int, BigInteger] = java.math.BigInteger.valueOf(_)
given long2bigInt as Conversion[Long, BigInteger] = java.math.BigInteger.valueOf(_)

def (a: Long) \:(b: Long) = BigInteger(a) \ b

type Rational = (BigInteger, BigInteger)
given Rational as RationalImpl

given id[T] as Conversion[T, T] = identity
given bigInt2rational[U](using Conversion[U, BigInteger]) as Conversion[U, Rational] = (_, 1)

def (a: Long) /:(b: Long) = Rational(a, b)

def println[T: Show](x: T) = System.out.println(x.toCode)

type ModInteger = scas.base.ModInteger

given coef2poly[U, C : Polynomial](using Conversion[U, C]) as Conversion[U, Polynomial.Element[C]] = summon[Polynomial[C]](_)

given infixAbelianGroupOps[U, T: AbelianGroup](using Conversion[U, T]) as Conversion[U, AbelianGroup.Ops[T]] = AbelianGroup.OpsImpl(_)

given infixSemiGroupOps[U, T: SemiGroup](using Conversion[U, T]) as Conversion[U, SemiGroup.Ops[T]] = SemiGroup.OpsImpl(_)

given infixRingOps[U, T: Ring](using Conversion[U, T]) as Conversion[U, Ring.Ops[T]] = Ring.OpsImpl(_)
