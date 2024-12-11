package scas.adapter.rings

import cc.redberry.rings.Rings
import scas.util.{Conversion, unary_~}
import cc.redberry.rings.bigint.BigInteger.valueOf

type BigInteger = cc.redberry.rings.bigint.BigInteger

object BigInteger extends BigInteger.Impl with Ring.Conv[BigInteger] {
  given instance: BigInteger.type = this
  abstract class Impl extends Ring[BigInteger] {
    def apply(str: String) = new BigInteger(str)
    val ring = Rings.Z
  }
  given int2bigInt: (Int => BigInteger) = valueOf(_)
  given long2bigInt: (Long => BigInteger) = valueOf(_)

  given bigInt2scas: [U : Conversion[BigInteger]] => (U => scas.base.BigInteger) = x => new scas.base.BigInteger((~x).toByteArray)
}
