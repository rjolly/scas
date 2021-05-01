package rings

import scas.util.{Conversion, unary_~}
import cc.redberry.rings.Rings

type BigInteger = cc.redberry.rings.bigint.BigInteger

object BigInteger extends Ring[BigInteger] {
  given BigInteger.type = this
  def apply(str: String) = new BigInteger(str)
  val ring = Rings.Z

  given int2bigInt: (Int => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)
  given long2bigInt: (Long => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)

  extension (a: Long) {
    def \ (b: Long): BigInteger = convert(a) \ b
    def \:(b: Long) = a \ b
  }

  given bigInt2scas[U: Conversion[BigInteger]]: (U => scas.base.BigInteger) = x => java.math.BigInteger((~x).toByteArray)
}
