package rings.conversion

import scas.util.{Conversion, unary_~}
import rings.BigInteger.Impl

type BigInteger = rings.BigInteger

object BigInteger extends Impl with Ring[BigInteger] {
  given BigInteger.type = this

  given int2bigInt: (Int => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)
  given long2bigInt: (Long => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)

  given bigInt2scas[U: Conversion[BigInteger]]: (U => scas.base.BigInteger) = x => java.math.BigInteger((~x).toByteArray)
}
