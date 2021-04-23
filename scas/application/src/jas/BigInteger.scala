package jas

import scas.util.{Conversion, unary_~}

type BigInteger = edu.jas.arith.BigInteger

object BigInteger extends Ring[BigInteger] {
  given BigInteger.type = this
  val factory = new BigInteger()
  def apply(str: String) = new BigInteger(str)

  given int2bigInt: (Int => BigInteger) = new BigInteger(_)
  given long2bigInt: (Long => BigInteger) = new BigInteger(_)

  extension (a: Long) def \:(b: Long) = BigInteger(a) \ b

  given bigInt2scas[U: Conversion[BigInteger]]: (U => scas.base.BigInteger) = x => (~x).`val`
}
