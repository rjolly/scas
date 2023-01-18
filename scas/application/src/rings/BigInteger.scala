package rings

import cc.redberry.rings.Rings
import scas.util.{Conversion, unary_~}

type BigInteger = cc.redberry.rings.bigint.BigInteger

object BigInteger extends Ring[BigInteger] {
  given BigInteger.type = this
  def apply(str: String) = new BigInteger(str)
  val ring = Rings.Z

  given int2bigInt: (Int => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)
  given long2bigInt: (Long => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)

  given bigInt2scas[U : Conversion[BigInteger]]: (U => scas.base.BigInteger) = x => java.math.BigInteger((~x).toByteArray)
}
