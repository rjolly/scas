package rings

import scala.util.FromDigits
import cc.redberry.rings.Rings
import cc.redberry.rings.Integers

type BigInteger = cc.redberry.rings.bigint.BigInteger

given Z as Integers = Rings.Z

given BigInteger as RdbRing[BigInteger] with FromDigits[BigInteger] with
  def fromDigits(digits: String) = new BigInteger(digits)
  def apply(x: BigInteger) = x

given id[T] as Conversion[T, T] = identity
given int2bigInt as Conversion[Int, BigInteger] = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given long2bigInt as Conversion[Long, BigInteger] = cc.redberry.rings.bigint.BigInteger.valueOf(_)
given bigInt2scas[U](using Conversion[U, BigInteger]) as Conversion[U, scas.BigInteger] = (x: U) => java.math.BigInteger(x.toByteArray)

def (a: Long) \:(b: Long) = BigInteger(a) \ b
