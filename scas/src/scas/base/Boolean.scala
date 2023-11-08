package scas.base

import scas.structure.BooleanRing
import scas.structure.commutative.ordered.conversion.Field

object Boolean extends Boolean.Impl with Field[Int] with scas.structure.conversion.BooleanRing[Int] {
  given instance: Boolean.type = this
  class Impl extends ModInteger(BigInteger("2")) with BooleanRing[Int]
}
