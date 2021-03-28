package rings

import scala.util.FromDigits
import cc.redberry.rings.Rings
import cc.redberry.rings.Integers

type BigInteger = cc.redberry.rings.bigint.BigInteger

object BigInteger extends Ring[BigInteger] with FromDigits[BigInteger] {
  given BigInteger.type = this
  def fromDigits(digits: String) = new BigInteger(digits)
  def apply(str: String) = new BigInteger(str)
  val ring: Integers = Rings.Z

  given int2bigInt: (Int => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)
  given long2bigInt: (Long => BigInteger) = cc.redberry.rings.bigint.BigInteger.valueOf(_)

  extension (a: Long) def \:(b: Long) = long2bigInt(a) \ bigInt2scas.apply(b)

  given bigInt2scas[U](using c: U => BigInteger): (U => scas.base.BigInteger) = x => java.math.BigInteger(c(x).toByteArray)
}
