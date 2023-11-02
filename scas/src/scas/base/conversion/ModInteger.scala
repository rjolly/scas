package scas.base.conversion

import scas.structure.commutative.ordered.conversion.Field
import scas.base.BigInteger

class ModInteger(val mod: BigInteger) extends scas.base.ModInteger with Field[BigInteger] {
  given instance: ModInteger = this
}
