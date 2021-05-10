package jas.conversion

import scas.util.{Conversion, unary_~}
import jas.BigInteger.Impl

type BigInteger = jas.BigInteger

object BigInteger extends Impl with Ring[BigInteger] {
  given BigInteger.type = this

  given int2bigInt: (Int => BigInteger) = new BigInteger(_)
  given long2bigInt: (Long => BigInteger) = new BigInteger(_)

  given bigInt2scas[U: Conversion[BigInteger]]: (U => scas.base.BigInteger) = x => (~x).`val`
}
