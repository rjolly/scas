package scas.base.conversion

import scas.structure.commutative.ordered.conversion.EuclidianDomain
import scas.base.BigInteger.Impl

type BigInteger = scas.base.BigInteger

object BigInteger extends Impl with EuclidianDomain[BigInteger] {
  given instance: BigInteger.type = this
  val characteristic = BigInteger("0")

  val zero = BigInteger("0")
  val one = BigInteger("1")

  given int2bigInt: (Int => BigInteger) = this(_)
  given long2bigInt: (Long => BigInteger) = this(_)
}
