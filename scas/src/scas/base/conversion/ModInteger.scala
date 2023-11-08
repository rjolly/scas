package scas.base.conversion

import scas.structure.commutative.ordered.conversion.Field
import scas.base.BigInteger

class ModInteger(mod: BigInteger) extends scas.base.ModInteger(mod) with Field[Int] {
  given instance: ModInteger = this
}
