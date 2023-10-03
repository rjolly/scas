package jas

import scas.base.BigInteger.given
import scas.util.{Conversion, unary_~}

type BigInteger = edu.jas.arith.BigInteger

object BigInteger extends BigInteger.Impl with Ring[BigInteger] {
  given instance: BigInteger.type = this
  abstract class Impl extends impl.Ring[BigInteger] {
    given instance: Impl
    val self: Impl = this
    val factory = new BigInteger()
    def apply(str: String) = new BigInteger(str)
  }
  given int2bigInt: (Int => BigInteger) = new BigInteger(_)
  given long2bigInt: (Long => BigInteger) = new BigInteger(_)

  given bigInt2scas[U : Conversion[BigInteger]]: (U => scas.base.BigInteger) = x => (~x).`val`
}
