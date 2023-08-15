package jas

import scas.util.{Conversion, unary_~}

type BigInteger = edu.jas.arith.BigInteger

object BigInteger extends Ring[BigInteger] {
  class Ops(using BigInteger.type) extends Ring.Ops[BigInteger]
  given BigInteger.type = this
  given Ops = new Ops
  val factory = new BigInteger()
  def apply(str: String) = new BigInteger(str)

  given int2bigInt: (Int => BigInteger) = new BigInteger(_)
  given long2bigInt: (Long => BigInteger) = new BigInteger(_)

  given bigInt2scas[U : Conversion[BigInteger]]: (U => scas.base.BigInteger) = x => (~x).`val`
}
