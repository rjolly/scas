package scas

import scas.base.{BigInteger, Rational}
import scas.prettyprint.Show

given bigInt2rational[U](using c: U => BigInteger): (U => Rational) = x => Rational.fromRing(c(x))

def println[T: Show](x: T) = System.out.println(x.show)
